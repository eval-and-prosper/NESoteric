const std = @import("std");
const testing = std.testing;

const Bus = @import("bus.zig").Bus;

pub const Cpu = struct {
    accumulator: u8 = 0,
    xRegister: u8 = 0,
    yRegister: u8 = 0,
    programCounter: u16 = 0,
    stackPointer: u8 = 0,
    statusRegister: Status = .{},
    cycles: u64 = 0,
    retiredInstructions: u64 = 0,

    pub fn powerOn(self: *Cpu, bus: *Bus) void {
        self.accumulator = 0;
        self.xRegister = 0;
        self.yRegister = 0;
        self.programCounter = bus.read16(0xFFFC);
        self.stackPointer = 0xFD;
        self.statusRegister = .{
            .interrupt_disable = true,
            .unused = true,
        };
        self.cycles = 7;
    }

    pub fn reset(self: *Cpu, bus: *Bus) void {
        self.stackPointer -%= 3;
        self.statusRegister.interrupt_disable = true;
        self.programCounter = bus.read16(0xFFFC);
        self.cycles += 7;
    }

    pub fn step(self: *Cpu, bus: *Bus) void {
        const op = bus.read(self.programCounter);
        // std.debug.print("Opcode: 0x{X:0>2}\n", .{op});
        self.incPC();
        execute(self, bus, op);
    }

    pub fn incPC(self: *Cpu) void {
        self.programCounter +%= 1;
    }

    pub fn incPcBy(self: *Cpu, amount: u16) void {
        self.programCounter +%= amount;
    }

    pub fn decPc(self: *Cpu) void {
        self.programCounter -%= 1;
    }

    pub fn getSpAddr(self: *Cpu) u16 {
        return 0x100 | @as(u16, self.stackPointer);
    }

    pub fn incSp(self: *Cpu) void {
        self.stackPointer +%= 1;
    }

    pub fn decSp(self: *Cpu) void {
        self.stackPointer -%= 1;
    }

    pub fn updateFlagsZN(self: *Cpu, value: u8) void {
        self.statusRegister.zero = value == 0;
        self.statusRegister.negative = (value & 0x80) != 0;
    }

    pub fn updateFlagsZ(self: *Cpu, value: u8) void {
        self.statusRegister.zero = value == 0;
    }

    pub fn updateFlagsN(self: *Cpu, value: u8) void {
        self.statusRegister.negative = (value & 0x80) != 0;
    }

    pub fn getOpcode(opcode: []const u8, mode: AddressingMode) u8 {
        var buf: [10]u8 = undefined;
        const upper = std.ascii.upperString(&buf, opcode);
        inline for (instructions) |inst| {
            if (std.mem.eql(u8, upper, inst.mnemonic) and mode == inst.mode) {
                return inst.opcode;
            }
        }
        std.debug.panic("Invalid Opcode mnemonic {s}", .{opcode});
    }

    pub fn print(self: *Cpu) void {
        std.debug.print("A {X:0>2} X {X:0>2} Y {X:0>2} PC {X:0>4} SP {X:0>2} ST {X:0>2} [{c}{c}{c}{c}{c}{c}] CYC {d}\n", .{
            self.accumulator,
            self.xRegister,
            self.yRegister,
            self.programCounter,
            self.stackPointer,
            @as(u8, @bitCast(self.statusRegister)),
            @as(u8, if (self.statusRegister.negative) 'N' else '-'),
            @as(u8, if (self.statusRegister.overflow) 'V' else '-'),
            @as(u8, if (self.statusRegister.decimal) 'D' else '-'),
            @as(u8, if (self.statusRegister.interrupt_disable) 'I' else '-'),
            @as(u8, if (self.statusRegister.zero) 'Z' else '-'),
            @as(u8, if (self.statusRegister.carry) 'C' else '-'),
            self.cycles,
        });
    }
};

const Status = packed struct(u8) {
    carry: bool = false,
    zero: bool = false,
    interrupt_disable: bool = true,
    decimal: bool = false,
    break_cmd: bool = false,
    unused: bool = true,
    overflow: bool = false,
    negative: bool = false,
};

pub const AddressingMode = enum {
    implied,
    implied_bus,
    accumulator,
    relative,
    immediate,
    absolute,
    zero_page,
    absolute_x,
    absolute_y,
    zero_page_x,
    zero_page_y,
    indirect,
    indirect_bug,
    indirect_x,
    indirect_y,
};

const InstructionDef = struct {
    opcode: u8,
    mnemonic: []const u8,
    mode: AddressingMode,
    bytes: u8,
    cycles: u8,
    page_cross_penalty: bool,
    handler: HandlerFn,
};

const Operand = union(enum) {
    accumulator,
    address: u16,
};

const AddressResult = struct {
    addr: u16,
    page_crossed: bool,
};

const HandlerFn = union(enum) {
    implied: *const fn (*Cpu) void,
    implied_bus: *const fn (*Cpu, *Bus) void,
    relative: *const fn (*Cpu, i8) void,
    operand: *const fn (*Cpu, *Bus, Operand) void,
};

fn instruction(
    comptime opcode: u8,
    comptime mnemonic: []const u8,
    comptime mode: AddressingMode,
    comptime bytes: u8,
    comptime cycles: u8,
    comptime handler: anytype,
) InstructionDef {
    return instructionWithPenalty(opcode, mnemonic, mode, bytes, cycles, false, handler);
}

fn instructionWithPenalty(
    comptime opcode: u8,
    comptime mnemonic: []const u8,
    comptime mode: AddressingMode,
    comptime bytes: u8,
    comptime cycles: u8,
    comptime page_cross_penalty: bool,
    comptime handler: anytype,
) InstructionDef {
    return .{
        .opcode = opcode,
        .mnemonic = mnemonic,
        .mode = mode,
        .bytes = bytes,
        .cycles = cycles,
        .page_cross_penalty = page_cross_penalty,
        .handler = switch (mode) {
            .implied => .{ .implied = handler },
            .implied_bus => .{ .implied_bus = handler },
            .relative => .{ .relative = handler },
            else => .{ .operand = handler },
        },
    };
}

// Wrapper for read-only instructions (ADC, AND, CMP, etc.)
// Core op receives value, updates CPU state
fn readOp(comptime op: fn (*Cpu, u8) void) *const fn (*Cpu, *Bus, Operand) void {
    return struct {
        fn handler(cpu: *Cpu, bus: *Bus, operand: Operand) void {
            const val = switch (operand) {
                .accumulator => cpu.accumulator,
                .address => |a| bus.read(a),
            };
            op(cpu, val);
        }
    }.handler;
}

// Wrapper for read-modify-write instructions (ASL, LSR, INC, etc.)
// Core op receives value, returns modified value
fn rmwOp(comptime op: fn (*Cpu, u8) u8) *const fn (*Cpu, *Bus, Operand) void {
    return struct {
        fn handler(cpu: *Cpu, bus: *Bus, operand: Operand) void {
            const val = switch (operand) {
                .accumulator => cpu.accumulator,
                .address => |a| bus.read(a),
            };
            const result = op(cpu, val);
            switch (operand) {
                .accumulator => cpu.accumulator = result,
                .address => |a| bus.write(a, result),
            }
        }
    }.handler;
}

// Wrapper for write-only instructions (STA, STX, STY)
fn writeOp(comptime reg: enum { a, x, y }) *const fn (*Cpu, *Bus, Operand) void {
    return struct {
        fn handler(cpu: *Cpu, bus: *Bus, operand: Operand) void {
            const val = switch (reg) {
                .a => cpu.accumulator,
                .x => cpu.xRegister,
                .y => cpu.yRegister,
            };
            switch (operand) {
                .accumulator => unreachable,
                .address => |addr| bus.write(addr, val),
            }
        }
    }.handler;
}

const instructions = [_]InstructionDef{
    instruction(0x69, "ADC", .immediate, 2, 2, readOp(adc)),
    instruction(0x65, "ADC", .zero_page, 2, 3, readOp(adc)),
    instruction(0x75, "ADC", .zero_page_x, 2, 4, readOp(adc)),
    instruction(0x6D, "ADC", .absolute, 3, 4, readOp(adc)),
    instructionWithPenalty(0x7D, "ADC", .absolute_x, 3, 4, true, readOp(adc)),
    instructionWithPenalty(0x79, "ADC", .absolute_y, 3, 4, true, readOp(adc)),
    instruction(0x61, "ADC", .indirect_x, 2, 6, readOp(adc)),
    instructionWithPenalty(0x71, "ADC", .indirect_y, 2, 5, true, readOp(adc)),

    instruction(0x29, "AND", .immediate, 2, 2, readOp(andOp)),
    instruction(0x25, "AND", .zero_page, 2, 3, readOp(andOp)),
    instruction(0x35, "AND", .zero_page_x, 2, 4, readOp(andOp)),
    instruction(0x2D, "AND", .absolute, 3, 4, readOp(andOp)),
    instructionWithPenalty(0x3D, "AND", .absolute_x, 3, 4, true, readOp(andOp)),
    instructionWithPenalty(0x39, "AND", .absolute_y, 3, 4, true, readOp(andOp)),
    instruction(0x21, "AND", .indirect_x, 2, 6, readOp(andOp)),
    instructionWithPenalty(0x31, "AND", .indirect_y, 2, 5, true, readOp(andOp)),

    instruction(0x0A, "ASL", .accumulator, 1, 2, rmwOp(asl)),
    instruction(0x06, "ASL", .zero_page, 2, 5, rmwOp(asl)),
    instruction(0x16, "ASL", .zero_page_x, 2, 6, rmwOp(asl)),
    instruction(0x0E, "ASL", .absolute, 3, 6, rmwOp(asl)),
    instruction(0x1E, "ASL", .absolute_x, 3, 7, rmwOp(asl)),

    instruction(0x90, "BCC", .relative, 2, 2, bcc),

    instruction(0xB0, "BCS", .relative, 2, 2, bcs),

    instruction(0xF0, "BEQ", .relative, 2, 2, beq),

    instruction(0x24, "BIT", .zero_page, 2, 3, readOp(bit)),
    instruction(0x2C, "BIT", .absolute, 3, 4, readOp(bit)),

    instruction(0x30, "BMI", .relative, 2, 2, bmi),

    instruction(0xD0, "BNE", .relative, 2, 2, bne),

    instruction(0x10, "BPL", .relative, 2, 2, bpl),

    instruction(0x00, "BRK", .implied_bus, 1, 7, brk),

    instruction(0x50, "BVC", .relative, 2, 2, bvc),
    instruction(0x70, "BVS", .relative, 2, 2, bvs),

    instruction(0x18, "CLC", .implied, 1, 2, clc),
    instruction(0xD8, "CLD", .implied, 1, 2, cld),
    instruction(0x58, "CLI", .implied, 1, 2, cli),
    instruction(0xB8, "CLV", .implied, 1, 2, clv),

    instruction(0xC9, "CMP", .immediate, 2, 2, readOp(cmp)),
    instruction(0xC5, "CMP", .zero_page, 2, 3, readOp(cmp)),
    instruction(0xD5, "CMP", .zero_page_x, 2, 4, readOp(cmp)),
    instruction(0xCD, "CMP", .absolute, 3, 4, readOp(cmp)),
    instructionWithPenalty(0xDD, "CMP", .absolute_x, 3, 4, true, readOp(cmp)),
    instructionWithPenalty(0xD9, "CMP", .absolute_y, 3, 4, true, readOp(cmp)),
    instruction(0xC1, "CMP", .indirect_x, 2, 6, readOp(cmp)),
    instructionWithPenalty(0xD1, "CMP", .indirect_y, 2, 5, true, readOp(cmp)),

    instruction(0xE0, "CPX", .immediate, 2, 2, readOp(cpx)),
    instruction(0xE4, "CPX", .zero_page, 2, 3, readOp(cpx)),
    instruction(0xEC, "CPX", .absolute, 3, 4, readOp(cpx)),

    instruction(0xC0, "CPY", .immediate, 2, 2, readOp(cpy)),
    instruction(0xC4, "CPY", .zero_page, 2, 3, readOp(cpy)),
    instruction(0xCC, "CPY", .absolute, 3, 4, readOp(cpy)),

    instruction(0xC6, "DEC", .zero_page, 2, 5, rmwOp(dec)),
    instruction(0xD6, "DEC", .zero_page_x, 2, 6, rmwOp(dec)),
    instruction(0xCE, "DEC", .absolute, 3, 6, rmwOp(dec)),
    instruction(0xDE, "DEC", .absolute_x, 3, 7, rmwOp(dec)),

    instruction(0xCA, "DEX", .implied, 1, 2, dex),
    instruction(0x88, "DEY", .implied, 1, 2, dey),

    instruction(0x49, "EOR", .immediate, 2, 2, readOp(eor)),
    instruction(0x45, "EOR", .zero_page, 2, 3, readOp(eor)),
    instruction(0x55, "EOR", .zero_page_x, 2, 4, readOp(eor)),
    instruction(0x4D, "EOR", .absolute, 3, 4, readOp(eor)),
    instructionWithPenalty(0x5D, "EOR", .absolute_x, 3, 4, true, readOp(eor)),
    instructionWithPenalty(0x59, "EOR", .absolute_y, 3, 4, true, readOp(eor)),
    instruction(0x41, "EOR", .indirect_x, 2, 6, readOp(eor)),
    instructionWithPenalty(0x51, "EOR", .indirect_y, 2, 5, true, readOp(eor)),

    instruction(0xE6, "INC", .zero_page, 2, 5, rmwOp(inc)),
    instruction(0xF6, "INC", .zero_page_x, 2, 6, rmwOp(inc)),
    instruction(0xEE, "INC", .absolute, 3, 6, rmwOp(inc)),
    instruction(0xFE, "INC", .absolute_x, 3, 7, rmwOp(inc)),

    instruction(0xE8, "INX", .implied, 1, 2, inx),
    instruction(0xC8, "INY", .implied, 1, 2, iny),

    instruction(0x4C, "JMP", .absolute, 3, 3, jmp),
    instruction(0x6C, "JMP", .indirect_bug, 3, 5, jmp),

    instruction(0x20, "JSR", .absolute, 3, 6, jsr),

    instruction(0xA9, "LDA", .immediate, 2, 2, readOp(lda)),
    instruction(0xA5, "LDA", .zero_page, 2, 3, readOp(lda)),
    instruction(0xB5, "LDA", .zero_page_x, 2, 4, readOp(lda)),
    instruction(0xAD, "LDA", .absolute, 3, 4, readOp(lda)),
    instructionWithPenalty(0xBD, "LDA", .absolute_x, 3, 4, true, readOp(lda)),
    instructionWithPenalty(0xB9, "LDA", .absolute_y, 3, 4, true, readOp(lda)),
    instruction(0xA1, "LDA", .indirect_x, 2, 6, readOp(lda)),
    instructionWithPenalty(0xB1, "LDA", .indirect_y, 2, 5, true, readOp(lda)),

    instruction(0xA2, "LDX", .immediate, 2, 2, readOp(ldx)),
    instruction(0xA6, "LDX", .zero_page, 2, 3, readOp(ldx)),
    instruction(0xB6, "LDX", .zero_page_y, 2, 4, readOp(ldx)),
    instruction(0xAE, "LDX", .absolute, 3, 4, readOp(ldx)),
    instructionWithPenalty(0xBE, "LDX", .absolute_y, 3, 4, true, readOp(ldx)),

    instruction(0xA0, "LDY", .immediate, 2, 2, readOp(ldy)),
    instruction(0xA4, "LDY", .zero_page, 2, 3, readOp(ldy)),
    instruction(0xB4, "LDY", .zero_page_x, 2, 4, readOp(ldy)),
    instruction(0xAC, "LDY", .absolute, 3, 4, readOp(ldy)),
    instructionWithPenalty(0xBC, "LDY", .absolute_x, 3, 4, true, readOp(ldy)),

    instruction(0x4A, "LSR", .accumulator, 1, 2, rmwOp(lsr)),
    instruction(0x46, "LSR", .zero_page, 2, 5, rmwOp(lsr)),
    instruction(0x56, "LSR", .zero_page_x, 2, 6, rmwOp(lsr)),
    instruction(0x4E, "LSR", .absolute, 3, 6, rmwOp(lsr)),
    instruction(0x5E, "LSR", .absolute_x, 3, 7, rmwOp(lsr)),

    instruction(0xEA, "NOP", .implied, 1, 2, nop),

    instruction(0x09, "ORA", .immediate, 2, 2, readOp(ora)),
    instruction(0x05, "ORA", .zero_page, 2, 3, readOp(ora)),
    instruction(0x15, "ORA", .zero_page_x, 2, 4, readOp(ora)),
    instruction(0x0D, "ORA", .absolute, 3, 4, readOp(ora)),
    instructionWithPenalty(0x1D, "ORA", .absolute_x, 3, 4, true, readOp(ora)),
    instructionWithPenalty(0x19, "ORA", .absolute_y, 3, 4, true, readOp(ora)),
    instruction(0x01, "ORA", .indirect_x, 2, 6, readOp(ora)),
    instructionWithPenalty(0x11, "ORA", .indirect_y, 2, 5, true, readOp(ora)),

    instruction(0x48, "PHA", .implied_bus, 1, 3, pha),
    instruction(0x08, "PHP", .implied_bus, 1, 3, php),
    instruction(0x68, "PLA", .implied_bus, 1, 4, pla),
    instruction(0x28, "PLP", .implied_bus, 1, 4, plp),

    instruction(0x2A, "ROL", .accumulator, 1, 2, rmwOp(rol)),
    instruction(0x26, "ROL", .zero_page, 2, 5, rmwOp(rol)),
    instruction(0x36, "ROL", .zero_page_x, 2, 6, rmwOp(rol)),
    instruction(0x2E, "ROL", .absolute, 3, 6, rmwOp(rol)),
    instruction(0x3E, "ROL", .absolute_x, 3, 7, rmwOp(rol)),

    instruction(0x6A, "ROR", .accumulator, 1, 2, rmwOp(ror)),
    instruction(0x66, "ROR", .zero_page, 2, 5, rmwOp(ror)),
    instruction(0x76, "ROR", .zero_page_x, 2, 6, rmwOp(ror)),
    instruction(0x6E, "ROR", .absolute, 3, 6, rmwOp(ror)),
    instruction(0x7E, "ROR", .absolute_x, 3, 7, rmwOp(ror)),

    instruction(0x40, "RTI", .implied_bus, 1, 6, rti),
    instruction(0x60, "RTS", .implied_bus, 1, 6, rts),

    instruction(0xE9, "SBC", .immediate, 2, 2, readOp(sbc)),
    instruction(0xE5, "SBC", .zero_page, 2, 3, readOp(sbc)),
    instruction(0xF5, "SBC", .zero_page_x, 2, 4, readOp(sbc)),
    instruction(0xED, "SBC", .absolute, 3, 4, readOp(sbc)),
    instructionWithPenalty(0xFD, "SBC", .absolute_x, 3, 4, true, readOp(sbc)),
    instructionWithPenalty(0xF9, "SBC", .absolute_y, 3, 4, true, readOp(sbc)),
    instruction(0xE1, "SBC", .indirect_x, 2, 6, readOp(sbc)),
    instructionWithPenalty(0xF1, "SBC", .indirect_y, 2, 5, true, readOp(sbc)),

    instruction(0x38, "SEC", .implied, 1, 2, sec),
    instruction(0xF8, "SED", .implied, 1, 2, sed),
    instruction(0x78, "SEI", .implied, 1, 2, sei),

    instruction(0x85, "STA", .zero_page, 2, 3, writeOp(.a)),
    instruction(0x95, "STA", .zero_page_x, 2, 4, writeOp(.a)),
    instruction(0x8D, "STA", .absolute, 3, 4, writeOp(.a)),
    instruction(0x9D, "STA", .absolute_x, 3, 5, writeOp(.a)),
    instruction(0x99, "STA", .absolute_y, 3, 5, writeOp(.a)),
    instruction(0x81, "STA", .indirect_x, 2, 6, writeOp(.a)),
    instruction(0x91, "STA", .indirect_y, 2, 6, writeOp(.a)),

    instruction(0x86, "STX", .zero_page, 2, 3, writeOp(.x)),
    instruction(0x96, "STX", .zero_page_y, 2, 4, writeOp(.x)),
    instruction(0x8E, "STX", .absolute, 3, 4, writeOp(.x)),

    instruction(0x84, "STY", .zero_page, 2, 3, writeOp(.y)),
    instruction(0x94, "STY", .zero_page_x, 2, 4, writeOp(.y)),
    instruction(0x8C, "STY", .absolute, 3, 4, writeOp(.y)),

    instruction(0xAA, "TAX", .implied, 1, 2, tax),
    instruction(0xA8, "TAY", .implied, 1, 2, tay),
    instruction(0xBA, "TSX", .implied, 1, 2, tsx),
    instruction(0x8A, "TXA", .implied, 1, 2, txa),
    instruction(0x9A, "TXS", .implied, 1, 2, txs),
    instruction(0x98, "TYA", .implied, 1, 2, tya),
};

// Core instruction operations - just the logic, no addressing
fn adc(cpu: *Cpu, val: u8) void {
    const a = cpu.accumulator;
    const carry = cpu.statusRegister.carry;
    const result: u16 = @as(u16, a) + val + @intFromBool(carry);
    const final: u8 = @truncate(result);

    cpu.statusRegister.carry = result > 0xFF;
    cpu.statusRegister.overflow = ((final ^ a) & (final ^ val) & 0x80) != 0;
    cpu.updateFlagsZN(final);
    cpu.accumulator = final;
}

fn andOp(cpu: *Cpu, val: u8) void {
    cpu.accumulator &= val;
    cpu.updateFlagsZN(cpu.accumulator);
}

fn asl(cpu: *Cpu, val: u8) u8 {
    const result = val << 1;
    cpu.statusRegister.carry = (val & 0x80) != 0;
    cpu.updateFlagsZN(result);
    return result;
}

fn bcc(cpu: *Cpu, offset: i8) void {
    if (!cpu.statusRegister.carry) {
        const old_page = cpu.programCounter & 0xFF00;
        cpu.programCounter +%= @bitCast(@as(i16, offset));
        cpu.cycles += 1; // taken
        if ((cpu.programCounter & 0xFF00) != old_page) cpu.cycles += 1; // page cross
    }
}

fn bcs(cpu: *Cpu, offset: i8) void {
    if (cpu.statusRegister.carry) {
        const old_page = cpu.programCounter & 0xFF00;
        cpu.programCounter +%= @bitCast(@as(i16, offset));
        cpu.cycles += 1; // taken
        if ((cpu.programCounter & 0xFF00) != old_page) cpu.cycles += 1; // page cross
    }
}

fn beq(cpu: *Cpu, offset: i8) void {
    if (cpu.statusRegister.zero) {
        const old_page = cpu.programCounter & 0xFF00;
        cpu.programCounter +%= @bitCast(@as(i16, offset));
        cpu.cycles += 1; // taken
        if ((cpu.programCounter & 0xFF00) != old_page) cpu.cycles += 1; // page cross
    }
}

fn bit(cpu: *Cpu, val: u8) void {
    const result = cpu.accumulator & val;
    cpu.statusRegister.overflow = (0x40 & val) != 0;
    cpu.updateFlagsZ(result);
    cpu.updateFlagsN(val);
}

fn bmi(cpu: *Cpu, offset: i8) void {
    if (cpu.statusRegister.negative) {
        const old_page = cpu.programCounter & 0xFF00;
        cpu.programCounter +%= @bitCast(@as(i16, offset));
        cpu.cycles += 1; // taken
        if ((cpu.programCounter & 0xFF00) != old_page) cpu.cycles += 1; // page cross
    }
}

fn bne(cpu: *Cpu, offset: i8) void {
    if (!cpu.statusRegister.zero) {
        const old_page = cpu.programCounter & 0xFF00;
        cpu.programCounter +%= @bitCast(@as(i16, offset));
        cpu.cycles += 1; // taken
        if ((cpu.programCounter & 0xFF00) != old_page) cpu.cycles += 1; // page cross
    }
}

fn bpl(cpu: *Cpu, offset: i8) void {
    if (!cpu.statusRegister.negative) {
        const old_page = cpu.programCounter & 0xFF00;
        cpu.programCounter +%= @bitCast(@as(i16, offset));
        cpu.cycles += 1; // taken
        if ((cpu.programCounter & 0xFF00) != old_page) cpu.cycles += 1; // page cross
    }
}

fn brk(cpu: *Cpu, bus: *Bus) void {
    cpu.incPC();
    // push pc
    bus.write(cpu.getSpAddr(), @truncate(cpu.programCounter >> 8));
    cpu.decSp();
    bus.write(cpu.getSpAddr(), @truncate(cpu.programCounter));
    cpu.decSp();
    // push flags
    php(cpu, bus);

    // set interrupt flag
    cpu.statusRegister.interrupt_disable = true;

    // fetch irq handler address
    const irq = bus.read16(0xFFFE);
    // set irq handler to pc
    cpu.programCounter = irq;
}

fn bvc(cpu: *Cpu, offset: i8) void {
    if (!cpu.statusRegister.overflow) {
        const old_page = cpu.programCounter & 0xFF00;
        cpu.programCounter +%= @bitCast(@as(i16, offset));
        cpu.cycles += 1; // taken
        if ((cpu.programCounter & 0xFF00) != old_page) cpu.cycles += 1; // page cross
    }
}

fn bvs(cpu: *Cpu, offset: i8) void {
    if (cpu.statusRegister.overflow) {
        const old_page = cpu.programCounter & 0xFF00;
        cpu.programCounter +%= @bitCast(@as(i16, offset));
        cpu.cycles += 1; // taken
        if ((cpu.programCounter & 0xFF00) != old_page) cpu.cycles += 1; // page cross
    }
}

fn clc(cpu: *Cpu) void {
    cpu.statusRegister.carry = false;
}

fn cld(cpu: *Cpu) void {
    cpu.statusRegister.decimal = false;
}

fn cli(cpu: *Cpu) void {
    cpu.statusRegister.interrupt_disable = false;
}

fn clv(cpu: *Cpu) void {
    cpu.statusRegister.overflow = false;
}

fn cmpSetFlags(cpu: *Cpu, mem: u8, reg: u8) void {
    // cpu.statusRegister.carry = cpu.accumulator >= value;
    // cpu.updateFlagsZN(result);
    //
    cpu.statusRegister.carry = reg >= mem;
    cpu.statusRegister.zero = reg == mem;
    cpu.statusRegister.negative = (reg -% mem) & 0x80 != 0;
}

fn cmp(cpu: *Cpu, value: u8) void {
    // const result = cpu.accumulator -% value;
    // cmpSetFlags(cpu, result, value);
    cmpSetFlags(cpu, value, cpu.accumulator);
}

fn cpx(cpu: *Cpu, value: u8) void {
    cmpSetFlags(cpu, value, cpu.xRegister);
}

fn cpy(cpu: *Cpu, value: u8) void {
    cmpSetFlags(cpu, value, cpu.yRegister);
}

fn dec(cpu: *Cpu, value: u8) u8 {
    const result = value -% 1;
    cpu.updateFlagsZN(result);
    return result;
}

fn dex(cpu: *Cpu) void {
    cpu.xRegister -%= 1;
    cpu.updateFlagsZN(cpu.xRegister);
}

fn dey(cpu: *Cpu) void {
    cpu.yRegister -%= 1;
    cpu.updateFlagsZN(cpu.yRegister);
}

fn eor(cpu: *Cpu, value: u8) void {
    cpu.accumulator ^= value;
    cpu.updateFlagsZN(cpu.accumulator);
}

fn inc(cpu: *Cpu, value: u8) u8 {
    const result = value +% 1;
    cpu.updateFlagsZN(result);
    return result;
}

fn inx(cpu: *Cpu) void {
    cpu.xRegister +%= 1;
    cpu.updateFlagsZN(cpu.xRegister);
}

fn iny(cpu: *Cpu) void {
    cpu.yRegister +%= 1;
    cpu.updateFlagsZN(cpu.yRegister);
}

fn jmp(cpu: *Cpu, _: *Bus, operand: Operand) void {
    cpu.programCounter = operand.address;
}

fn jsr(cpu: *Cpu, bus: *Bus, operand: Operand) void {
    const pc = cpu.programCounter -% 1;
    bus.write(cpu.getSpAddr(), @truncate(pc >> 8));
    cpu.decSp();
    bus.write(cpu.getSpAddr(), @truncate(pc));
    cpu.decSp();
    cpu.programCounter = operand.address;
}

fn lda(cpu: *Cpu, value: u8) void {
    cpu.accumulator = value;
    cpu.updateFlagsZN(value);
}

fn ldx(cpu: *Cpu, value: u8) void {
    cpu.xRegister = value;
    cpu.updateFlagsZN(value);
}

fn ldy(cpu: *Cpu, value: u8) void {
    cpu.yRegister = value;
    cpu.updateFlagsZN(value);
}

fn lsr(cpu: *Cpu, value: u8) u8 {
    const carry = value & 0x1;
    const result = (value >> 1);
    cpu.statusRegister.carry = carry != 0;
    cpu.updateFlagsZN(result);
    return result;
}

fn nop(_: *Cpu) void {}

fn ora(cpu: *Cpu, value: u8) void {
    cpu.accumulator |= value;
    cpu.updateFlagsZN(cpu.accumulator);
}

fn pha(cpu: *Cpu, bus: *Bus) void {
    bus.write(cpu.getSpAddr(), cpu.accumulator);
    cpu.decSp();
}

fn php(cpu: *Cpu, bus: *Bus) void {
    var flags = cpu.statusRegister;
    flags.break_cmd = true;
    flags.unused = true;
    bus.write(cpu.getSpAddr(), @bitCast(flags));
    cpu.decSp();
}

fn pla(cpu: *Cpu, bus: *Bus) void {
    cpu.incSp();
    const mem = bus.read(cpu.getSpAddr());
    cpu.accumulator = mem;
    cpu.updateFlagsZN(mem);
}

fn plp(cpu: *Cpu, bus: *Bus) void {
    cpu.incSp();
    const mem = bus.read(cpu.getSpAddr());
    const b = cpu.statusRegister.break_cmd;
    const u = cpu.statusRegister.unused;
    cpu.statusRegister = @bitCast(mem);
    cpu.statusRegister.break_cmd = b;
    cpu.statusRegister.unused = u;
}

fn rol(cpu: *Cpu, value: u8) u8 {
    const carryOut = value & 0x80;
    const carryIn = @intFromBool(cpu.statusRegister.carry);
    const result = carryIn +% (value << 1);
    cpu.statusRegister.carry = carryOut != 0;
    cpu.updateFlagsZN(result);
    return result;
}

fn ror(cpu: *Cpu, value: u8) u8 {
    const carryOut = value & 1;
    const carryIn: u8 = @intFromBool(cpu.statusRegister.carry);
    var result = value >> 1;
    result = result | (carryIn << 7);
    cpu.statusRegister.carry = carryOut != 0;
    cpu.updateFlagsZN(result);
    return result;
}

fn rti(cpu: *Cpu, bus: *Bus) void {
    plp(cpu, bus);
    rts(cpu, bus);
    cpu.decPc();
}

fn rts(cpu: *Cpu, bus: *Bus) void {
    cpu.incSp();
    const pcLow = bus.read(cpu.getSpAddr());
    cpu.incSp();
    const pcHigh = bus.read(cpu.getSpAddr());
    cpu.programCounter = ((@as(u16, pcHigh) << 8) | pcLow) +% 1;
}

fn sbc(cpu: *Cpu, value: u8) void {
    // const carry = @intFromBool(cpu.statusRegister.carry);
    // const a = cpu.accumulator;
    // const memNot = ~value;
    // const result16 = @as(u16, a) +% memNot +% carry;
    // const result: u8 = @truncate(result16);
    // cpu.statusRegister.carry = result16 > 0xFF;
    // cpu.statusRegister.overflow = ((result ^ a) & (result ^ memNot) & 0x80) != 0;
    // cpu.updateFlagsZN(result);
    // cpu.accumulator = result;
    adc(cpu, ~value);
}

fn sec(cpu: *Cpu) void {
    cpu.statusRegister.carry = true;
}

fn sed(cpu: *Cpu) void {
    cpu.statusRegister.decimal = true;
}

fn sei(cpu: *Cpu) void {
    cpu.statusRegister.interrupt_disable = true;
}

// fn sta(cpu: *Cpu, bus: *Bus, op: Operand) void {
// bus.write(op.address, cpu.accumulator);
// }

// fn stx(cpu: *Cpu, bus: *Bus, op: Operand) void {
// bus.write(op.address, cpu.xRegister);
// }

// fn sty(cpu: *Cpu, bus: *Bus, op: Operand) void {
// bus.write(op.address, cpu.yRegister);
// }

fn tax(cpu: *Cpu) void {
    cpu.xRegister = cpu.accumulator;
    cpu.updateFlagsZN(cpu.xRegister);
}

fn tay(cpu: *Cpu) void {
    cpu.yRegister = cpu.accumulator;
    cpu.updateFlagsZN(cpu.yRegister);
}

fn tsx(cpu: *Cpu) void {
    cpu.xRegister = cpu.stackPointer;
    cpu.updateFlagsZN(cpu.xRegister);
}

fn txa(cpu: *Cpu) void {
    cpu.accumulator = cpu.xRegister;
    cpu.updateFlagsZN(cpu.accumulator);
}

fn txs(cpu: *Cpu) void {
    cpu.stackPointer = cpu.xRegister;
}

fn tya(cpu: *Cpu) void {
    cpu.accumulator = cpu.yRegister;
    cpu.updateFlagsZN(cpu.accumulator);
}

pub fn resolveAddress(cpu: *Cpu, bus: *Bus, mode: AddressingMode) AddressResult {
    return switch (mode) {
        .immediate => blk: {
            const addr = cpu.programCounter;
            cpu.incPC();
            break :blk .{ .addr = addr, .page_crossed = false };
        },
        .absolute => blk: {
            const pc = cpu.programCounter;
            const addr = bus.read16(pc);
            cpu.incPcBy(2);
            break :blk .{ .addr = addr, .page_crossed = false };
        },
        .zero_page => blk: {
            const pc = cpu.programCounter;
            const addr = bus.read(pc);
            cpu.incPC();
            break :blk .{ .addr = addr, .page_crossed = false };
        },
        .absolute_x => blk: {
            const pc = cpu.programCounter;
            const base = bus.read16(pc);
            const addr = base +% cpu.xRegister;
            cpu.incPcBy(2);
            break :blk .{ .addr = addr, .page_crossed = (base & 0xFF00) != (addr & 0xFF00) };
        },
        .absolute_y => blk: {
            const pc = cpu.programCounter;
            const base = bus.read16(pc);
            const addr = base +% cpu.yRegister;
            cpu.incPcBy(2);
            break :blk .{ .addr = addr, .page_crossed = (base & 0xFF00) != (addr & 0xFF00) };
        },
        .zero_page_x => blk: {
            const pc = cpu.programCounter;
            const zp = bus.read(pc);
            const addr = zp +% cpu.xRegister;
            cpu.incPC();
            break :blk .{ .addr = addr, .page_crossed = false };
        },
        .zero_page_y => blk: {
            const pc = cpu.programCounter;
            const zp = bus.read(pc);
            const addr = zp +% cpu.yRegister;
            cpu.incPC();
            break :blk .{ .addr = addr, .page_crossed = false };
        },
        .indirect => blk: {
            const pc = cpu.programCounter;
            const ptr = bus.read16(pc);
            const addr = bus.read16(ptr);
            cpu.incPcBy(2);
            break :blk .{ .addr = addr, .page_crossed = false };
        },
        .indirect_bug => blk: {
            // When least significant address byte ends in #xFF it reads
            // that value from XX00 instead of wrapping the high byte
            const pc = cpu.programCounter;
            const ptr = bus.read16(pc);
            const lo = bus.read(ptr);
            const hi_addr = (ptr & 0xFF00) | ((ptr +% 1) & 0x00FF);
            const hi = bus.read(hi_addr);
            cpu.incPcBy(2);
            break :blk .{ .addr = (@as(u16, hi) << 8) | lo, .page_crossed = false };
        },
        .indirect_x => blk: {
            // Pre-indexed indirect: (zp,X)
            // ZP address + X (wrapping in zero page), then read 16-bit address from there
            const pc = cpu.programCounter;
            const zpAddr = bus.read(pc) +% cpu.xRegister;
            const addr = bus.read16ZeroPage(zpAddr);
            cpu.incPC();
            break :blk .{ .addr = addr, .page_crossed = false };
        },
        .indirect_y => blk: {
            // Post-indexed indirect: (zp),Y
            // Read 16-bit base address from ZP, then add Y
            const pc = cpu.programCounter;
            const zpAddr = bus.read(pc);
            const baseAddr = bus.read16ZeroPage(zpAddr);
            const addr = baseAddr +% cpu.yRegister;
            cpu.incPC();
            break :blk .{ .addr = addr, .page_crossed = (baseAddr & 0xFF00) != (addr & 0xFF00) };
        },
        .implied, .implied_bus, .accumulator, .relative => unreachable,
    };
}

pub fn resolveRelativeOffset(cpu: *Cpu, bus: *Bus) i8 {
    const offset: i8 = @bitCast(bus.read(cpu.programCounter));
    cpu.incPC();
    return offset;
}

fn execute(cpu: *Cpu, bus: *Bus, opcode: u8) void {
    inline for (instructions) |inst| {
        if (opcode == inst.opcode) {
            var page_crossed = false;
            switch (inst.handler) {
                .implied => |h| h(cpu),
                .implied_bus => |h| h(cpu, bus),
                .relative => |h| {
                    const offset = resolveRelativeOffset(cpu, bus);
                    h(cpu, offset);
                },
                .operand => |h| {
                    const op: Operand = if (inst.mode == .accumulator)
                        .accumulator
                    else blk: {
                        const result = resolveAddress(cpu, bus, inst.mode);
                        page_crossed = result.page_crossed;
                        break :blk .{ .address = result.addr };
                    };
                    h(cpu, bus, op);
                },
            }
            cpu.cycles += inst.cycles;
            cpu.retiredInstructions += 1;
            if (inst.page_cross_penalty and page_crossed) cpu.cycles += 1;
            return;
        }
    }
    std.debug.panic("unknown opcode 0x{X}", .{opcode});
}