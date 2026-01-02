const std = @import("std");
const testing = std.testing;

const cpu = @import("cpu.zig");
const bus = @import("bus.zig");

const TestContext = struct {
    c: cpu.Cpu,
    b: bus.Bus,
};

fn setup() TestContext {
    var ctx = TestContext{
        .c = cpu.Cpu{},
        .b = bus.Bus{},
    };
    ctx.c.powerOn(&ctx.b);
    ctx.c.programCounter = 0;
    return ctx;
}

test "CPU ADC" {
    var ctx = setup();

    const adcImmediate = cpu.Cpu.getOpcode("ADC", .immediate);
    try testing.expectEqual(0x69, adcImmediate);

    // basic addition
    ctx.c.accumulator = 0x20;
    ctx.b.write(0, adcImmediate);
    ctx.b.write(1, 0x42);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x62, ctx.c.accumulator);

    // carry set
    ctx = setup();
    ctx.c.accumulator = 0x20;
    ctx.c.statusRegister.carry = true;
    ctx.b.write(0, adcImmediate);
    ctx.b.write(1, 0x42);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x63, ctx.c.accumulator);

    // carry
    ctx = setup();
    ctx.c.accumulator = 0xFF;
    ctx.b.write(0, adcImmediate);
    ctx.b.write(1, 0x01);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.carry);
    try testing.expectEqual(0x00, ctx.c.accumulator);

    // zero flag behavior
    ctx = setup();
    ctx.c.accumulator = 0x00;
    ctx.b.write(0, adcImmediate);
    ctx.b.write(1, 0x00);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);

    // negative flag behavior
    ctx = setup();
    ctx.c.accumulator = 0x50;
    ctx.b.write(0, adcImmediate);
    ctx.b.write(1, 0x90);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);

    // overflow
    ctx = setup();
    ctx.c.accumulator = 0x7F; // +127
    ctx.b.write(0, adcImmediate);
    ctx.b.write(1, 0x01); // +1
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.overflow); // 127 + 1 wraps to -128
    try testing.expectEqual(0x80, ctx.c.accumulator);

    // overflow: negative + negative = positive
    ctx = setup();
    ctx.c.accumulator = 0x80; // -128
    ctx.b.write(0, adcImmediate);
    ctx.b.write(1, 0x80); // -128
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.overflow);
    try testing.expectEqual(true, ctx.c.statusRegister.carry);
    try testing.expectEqual(0x00, ctx.c.accumulator);
}

test "CPU AND" {
    var ctx = setup();

    const andImmediate = cpu.Cpu.getOpcode("AND", .immediate);
    try testing.expectEqual(0x29, andImmediate);

    // basic operation
    ctx.c.accumulator = 0xF0;
    ctx.b.write(0, andImmediate);
    ctx.b.write(1, 0xAA);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0xA0, ctx.c.accumulator);

    // zero flag behavior
    ctx = setup();
    ctx.c.accumulator = 0xF0;
    ctx.b.write(0, andImmediate);
    ctx.b.write(1, 0x0F);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);

    // negative flag behavior
    ctx = setup();
    ctx.c.accumulator = 0xFF;
    ctx.b.write(0, andImmediate);
    ctx.b.write(1, 0x80);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);
}

test "CPU ASL" {
    var ctx = setup();
    const aslAcc = cpu.Cpu.getOpcode("ASL", .accumulator);

    // basic shift operation
    ctx.c.accumulator = 0x55;
    ctx.b.write(0, aslAcc);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x55 << 1, ctx.c.accumulator);

    // carry flag behavior
    ctx = setup();
    ctx.c.accumulator = 0x80;
    ctx.b.write(0, aslAcc);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.carry);
    try testing.expectEqual(0, ctx.c.accumulator);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);

    // negative flag behavior
    ctx = setup();
    ctx.c.accumulator = 0x40;
    ctx.b.write(0, aslAcc);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);
}

test "CPU BCC" {
    var ctx = setup();
    const bcc = cpu.Cpu.getOpcode("BCC", .relative);

    // test branch when carry clear
    ctx.c.statusRegister.carry = false;
    ctx.b.write(0, bcc);
    ctx.b.write(1, 0x05);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x07, ctx.c.programCounter);

    // test no branch when carry set
    ctx = setup();
    ctx.c.statusRegister.carry = true;
    ctx.b.write(0, bcc);
    ctx.b.write(1, 0x05);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x02, ctx.c.programCounter);

    // test negative branch offset
    ctx = setup();
    ctx.c.statusRegister.carry = false;
    ctx.b.write(0, bcc);
    ctx.b.write(1, 0xFA);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0xFFFC, ctx.c.programCounter);
}

test "CPU BCS" {
    var ctx = setup();
    const bcs = cpu.Cpu.getOpcode("BCS", .relative);

    // test branch when carry set
    ctx.c.statusRegister.carry = true;
    ctx.b.write(0, bcs);
    ctx.b.write(1, 0x05);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x07, ctx.c.programCounter);

    // test no branch when carry clear
    ctx = setup();
    ctx.c.statusRegister.carry = false;
    ctx.b.write(0, bcs);
    ctx.b.write(1, 0x05);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x02, ctx.c.programCounter);

    // test negative branch offset
    ctx = setup();
    ctx.c.statusRegister.carry = true;
    ctx.b.write(0, bcs);
    ctx.b.write(1, 0xFA);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0xFFFC, ctx.c.programCounter);
}

test "CPU BEQ" {
    var ctx = setup();
    const beq = cpu.Cpu.getOpcode("BEQ", .relative);

    // test branch when zero flag set
    ctx.c.statusRegister.zero = true;
    ctx.b.write(0, beq);
    ctx.b.write(1, 0x05);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x07, ctx.c.programCounter);

    // test no branch when zero flag clear
    ctx = setup();
    ctx.c.statusRegister.zero = false;
    ctx.b.write(0, beq);
    ctx.b.write(1, 0x05);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x02, ctx.c.programCounter);

    // test negative branch offset
    ctx = setup();
    ctx.c.statusRegister.zero = true;
    ctx.b.write(0, beq);
    ctx.b.write(1, 0xFA);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0xFFFC, ctx.c.programCounter);
}

test "CPU BIT" {
    var ctx = setup();
    const bitZp = cpu.Cpu.getOpcode("BIT", .zero_page);

    // Zero flag behavior with AND result
    ctx.c.accumulator = 0b10000000;
    ctx.b.write(0, bitZp);
    ctx.b.write(1, 0x50);
    ctx.b.write(0x50, 0b00000001);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);

    // Zero flag clear when bits match
    ctx = setup();
    ctx.c.accumulator = 0b00000001;
    ctx.b.write(0, bitZp);
    ctx.b.write(1, 0x50);
    ctx.b.write(0x50, 0b00000001);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(false, ctx.c.statusRegister.zero);

    // Negative flag from memory bit 7
    ctx = setup();
    ctx.c.accumulator = 0b00000001;
    ctx.b.write(0, bitZp);
    ctx.b.write(1, 0x50);
    ctx.b.write(0x50, 0b10000000);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);

    // Overflow flag from memory bit 6
    ctx = setup();
    ctx.c.accumulator = 0b00000001;
    ctx.b.write(0, bitZp);
    ctx.b.write(1, 0x50);
    ctx.b.write(0x50, 0b01000000);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.overflow);
}

test "CPU BMI" {
    var ctx = setup();
    const bmi = cpu.Cpu.getOpcode("BMI", .relative);

    // test branch when negative flag set
    ctx.c.statusRegister.negative = true;
    ctx.b.write(0, bmi);
    ctx.b.write(1, 0x05);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x07, ctx.c.programCounter);

    // test no branch when negative flag clear
    ctx = setup();
    ctx.c.statusRegister.negative = false;
    ctx.b.write(0, bmi);
    ctx.b.write(1, 0x05);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x02, ctx.c.programCounter);

    // test negative branch offset
    ctx = setup();
    ctx.c.statusRegister.negative = true;
    ctx.b.write(0, bmi);
    ctx.b.write(1, 0xFA);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0xFFFC, ctx.c.programCounter);
}

test "CPU BNE" {
    var ctx = setup();
    const bne = cpu.Cpu.getOpcode("BNE", .relative);

    // test branch when zero flag clear
    ctx.c.statusRegister.zero = false;
    ctx.b.write(0, bne);
    ctx.b.write(1, 0x05);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x07, ctx.c.programCounter);

    // test no branch when zero flag set
    ctx = setup();
    ctx.c.statusRegister.zero = true;
    ctx.b.write(0, bne);
    ctx.b.write(1, 0x05);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x02, ctx.c.programCounter);

    // test negative branch offset
    ctx = setup();
    ctx.c.statusRegister.zero = false;
    ctx.b.write(0, bne);
    ctx.b.write(1, 0xFA);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0xFFFC, ctx.c.programCounter);
}

test "CPU BRK" {
    var ctx = setup();
    const brk = cpu.Cpu.getOpcode("BRK", .implied_bus);

    // Setup IRQ vector
    ctx.b.write(0xFFFE, 0x34);
    ctx.b.write(0xFFFF, 0x12);

    // Basic BRK - jumps to IRQ vector
    ctx.c.programCounter = 0x0200;
    ctx.c.stackPointer = 0xFF;
    ctx.b.write(0x0200, brk);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x1234, ctx.c.programCounter);

    // Sets interrupt disable flag
    ctx = setup();
    ctx.c.programCounter = 0x0200;
    ctx.c.stackPointer = 0xFF;
    ctx.c.statusRegister.interrupt_disable = false;
    ctx.b.write(0xFFFE, 0x00);
    ctx.b.write(0xFFFF, 0x00);
    ctx.b.write(0x0200, brk);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.interrupt_disable);

    // Pushes return address (PC+2) to stack - high byte first
    ctx = setup();
    ctx.c.programCounter = 0x0200;
    ctx.c.stackPointer = 0xFF;
    ctx.b.write(0xFFFE, 0x00);
    ctx.b.write(0xFFFF, 0x00);
    ctx.b.write(0x0200, brk);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x02, ctx.b.read(0x01FF)); // High byte of 0x0202
    try testing.expectEqual(0x02, ctx.b.read(0x01FE)); // Low byte of 0x0202

    // Pushes status register with B and unused flags set
    ctx = setup();
    ctx.c.programCounter = 0x0200;
    ctx.c.stackPointer = 0xFF;
    ctx.c.statusRegister.negative = true;
    ctx.c.statusRegister.overflow = true;
    ctx.c.statusRegister.carry = true;
    ctx.b.write(0xFFFE, 0x00);
    ctx.b.write(0xFFFF, 0x00);
    ctx.b.write(0x0200, brk);
    ctx.c.step(&ctx.b);
    const pushedFlags = ctx.b.read(0x01FD);
    try testing.expectEqual(true, (pushedFlags & 0x10) != 0); // Break flag set
    try testing.expectEqual(true, (pushedFlags & 0x20) != 0); // Unused flag set
    try testing.expectEqual(true, (pushedFlags & 0x80) != 0); // Negative preserved
    try testing.expectEqual(true, (pushedFlags & 0x40) != 0); // Overflow preserved
    try testing.expectEqual(true, (pushedFlags & 0x01) != 0); // Carry preserved

    // Stack pointer decremented by 3 (2 for PC, 1 for flags)
    ctx = setup();
    ctx.c.programCounter = 0x0200;
    ctx.c.stackPointer = 0xFF;
    ctx.b.write(0xFFFE, 0x00);
    ctx.b.write(0xFFFF, 0x00);
    ctx.b.write(0x0200, brk);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0xFC, ctx.c.stackPointer);
}

test "CPU BVC" {
    var ctx = setup();
    const bvc = cpu.Cpu.getOpcode("BVC", .relative);

    // test branch when overflow flag clear
    ctx.c.statusRegister.overflow = false;
    ctx.b.write(0, bvc);
    ctx.b.write(1, 0x05);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x07, ctx.c.programCounter);

    // test no branch when overflow flag set
    ctx = setup();
    ctx.c.statusRegister.overflow = true;
    ctx.b.write(0, bvc);
    ctx.b.write(1, 0x05);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x02, ctx.c.programCounter);

    // test negative branch offset
    ctx = setup();
    ctx.c.statusRegister.overflow = false;
    ctx.b.write(0, bvc);
    ctx.b.write(1, 0xFA);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0xFFFC, ctx.c.programCounter);
}

test "CPU BVS" {
    var ctx = setup();
    const bvs = cpu.Cpu.getOpcode("BVS", .relative);

    // test branch when overflow flag set
    ctx.c.statusRegister.overflow = true;
    ctx.b.write(0, bvs);
    ctx.b.write(1, 0x05);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x07, ctx.c.programCounter);

    // test no branch when overflow flag clear
    ctx = setup();
    ctx.c.statusRegister.overflow = false;
    ctx.b.write(0, bvs);
    ctx.b.write(1, 0x05);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x02, ctx.c.programCounter);

    // test negative branch offset
    ctx = setup();
    ctx.c.statusRegister.overflow = true;
    ctx.b.write(0, bvs);
    ctx.b.write(1, 0xFA);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0xFFFC, ctx.c.programCounter);
}

test "CPU CLC" {
    var ctx = setup();
    const clc = cpu.Cpu.getOpcode("CLC", .implied);

    // Clear carry flag when set
    ctx.c.statusRegister.carry = true;
    ctx.b.write(0, clc);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(false, ctx.c.statusRegister.carry);

    // Keep carry flag clear when already clear
    ctx = setup();
    ctx.c.statusRegister.carry = false;
    ctx.b.write(0, clc);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(false, ctx.c.statusRegister.carry);

    // Doesn't affect other flags
    ctx = setup();
    ctx.c.statusRegister.zero = true;
    ctx.c.statusRegister.overflow = true;
    ctx.c.statusRegister.negative = true;
    ctx.c.statusRegister.carry = true;
    ctx.b.write(0, clc);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);
    try testing.expectEqual(true, ctx.c.statusRegister.overflow);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);
}

test "CPU CLD" {
    var ctx = setup();
    const cld = cpu.Cpu.getOpcode("CLD", .implied);

    // Clear decimal flag when set
    ctx.c.statusRegister.decimal = true;
    ctx.b.write(0, cld);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(false, ctx.c.statusRegister.decimal);

    // Keep decimal flag clear when already clear
    ctx = setup();
    ctx.c.statusRegister.decimal = false;
    ctx.b.write(0, cld);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(false, ctx.c.statusRegister.decimal);

    // Doesn't affect other flags
    ctx = setup();
    ctx.c.statusRegister.zero = true;
    ctx.c.statusRegister.overflow = true;
    ctx.c.statusRegister.negative = true;
    ctx.c.statusRegister.carry = true;
    ctx.c.statusRegister.decimal = true;
    ctx.b.write(0, cld);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);
    try testing.expectEqual(true, ctx.c.statusRegister.overflow);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);
    try testing.expectEqual(true, ctx.c.statusRegister.carry);
}

test "CPU CLI" {
    var ctx = setup();
    const cli = cpu.Cpu.getOpcode("CLI", .implied);

    // Clear interrupt flag when set
    ctx.c.statusRegister.interrupt_disable = true;
    ctx.b.write(0, cli);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(false, ctx.c.statusRegister.interrupt_disable);

    // Keep interrupt flag clear when already clear
    ctx = setup();
    ctx.c.statusRegister.interrupt_disable = false;
    ctx.b.write(0, cli);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(false, ctx.c.statusRegister.interrupt_disable);

    // Doesn't affect other flags
    ctx = setup();
    ctx.c.statusRegister.zero = true;
    ctx.c.statusRegister.overflow = true;
    ctx.c.statusRegister.negative = true;
    ctx.c.statusRegister.carry = true;
    ctx.c.statusRegister.decimal = true;
    ctx.c.statusRegister.interrupt_disable = true;
    ctx.b.write(0, cli);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);
    try testing.expectEqual(true, ctx.c.statusRegister.overflow);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);
    try testing.expectEqual(true, ctx.c.statusRegister.carry);
    try testing.expectEqual(true, ctx.c.statusRegister.decimal);
}

test "CPU CLV" {
    var ctx = setup();
    const clv = cpu.Cpu.getOpcode("CLV", .implied);

    // Clear overflow flag when set
    ctx.c.statusRegister.overflow = true;
    ctx.b.write(0, clv);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(false, ctx.c.statusRegister.overflow);

    // Keep overflow flag clear when already clear
    ctx = setup();
    ctx.c.statusRegister.overflow = false;
    ctx.b.write(0, clv);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(false, ctx.c.statusRegister.overflow);

    // Doesn't affect other flags
    ctx = setup();
    ctx.c.statusRegister.zero = true;
    ctx.c.statusRegister.negative = true;
    ctx.c.statusRegister.carry = true;
    ctx.c.statusRegister.decimal = true;
    ctx.c.statusRegister.interrupt_disable = true;
    ctx.c.statusRegister.overflow = true;
    ctx.b.write(0, clv);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);
    try testing.expectEqual(true, ctx.c.statusRegister.carry);
    try testing.expectEqual(true, ctx.c.statusRegister.decimal);
    try testing.expectEqual(true, ctx.c.statusRegister.interrupt_disable);
}

test "CPU CMP" {
    var ctx = setup();
    const cmpImmediate = cpu.Cpu.getOpcode("CMP", .immediate);

    // Test zero flag when accumulator equals memory
    ctx.c.accumulator = 0x42;
    ctx.b.write(0, cmpImmediate);
    ctx.b.write(1, 0x42);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);
    try testing.expectEqual(true, ctx.c.statusRegister.carry);

    // Test carry flag when accumulator greater than memory
    ctx = setup();
    ctx.c.accumulator = 0x42;
    ctx.b.write(0, cmpImmediate);
    ctx.b.write(1, 0x40);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.carry);
    try testing.expectEqual(false, ctx.c.statusRegister.zero);

    // Test negative flag when result has bit 7 set
    ctx = setup();
    ctx.c.accumulator = 0x40;
    ctx.b.write(0, cmpImmediate);
    ctx.b.write(1, 0xC0);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);
    try testing.expectEqual(false, ctx.c.statusRegister.carry);

    // Verify accumulator unchanged
    ctx = setup();
    ctx.c.accumulator = 0x42;
    ctx.b.write(0, cmpImmediate);
    ctx.b.write(1, 0x40);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x42, ctx.c.accumulator);
}

test "CPU CPX" {
    var ctx = setup();
    const cpxImmediate = cpu.Cpu.getOpcode("CPX", .immediate);

    // Test zero flag when X register equals memory
    ctx.c.xRegister = 0x42;
    ctx.b.write(0, cpxImmediate);
    ctx.b.write(1, 0x42);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);
    try testing.expectEqual(true, ctx.c.statusRegister.carry);

    // Test carry flag when X register greater than memory
    ctx = setup();
    ctx.c.xRegister = 0x42;
    ctx.b.write(0, cpxImmediate);
    ctx.b.write(1, 0x40);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.carry);
    try testing.expectEqual(false, ctx.c.statusRegister.zero);

    // Test negative flag when result has bit 7 set
    ctx = setup();
    ctx.c.xRegister = 0x40;
    ctx.b.write(0, cpxImmediate);
    ctx.b.write(1, 0xC0);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);
    try testing.expectEqual(false, ctx.c.statusRegister.carry);

    // Verify X register unchanged
    ctx = setup();
    ctx.c.xRegister = 0x42;
    ctx.b.write(0, cpxImmediate);
    ctx.b.write(1, 0x40);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x42, ctx.c.xRegister);
}

test "CPU CPY" {
    var ctx = setup();
    const cpyImmediate = cpu.Cpu.getOpcode("CPY", .immediate);

    // Test zero flag when Y register equals memory
    ctx.c.yRegister = 0x42;
    ctx.b.write(0, cpyImmediate);
    ctx.b.write(1, 0x42);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);
    try testing.expectEqual(true, ctx.c.statusRegister.carry);

    // Test carry flag when Y register greater than memory
    ctx = setup();
    ctx.c.yRegister = 0x42;
    ctx.b.write(0, cpyImmediate);
    ctx.b.write(1, 0x40);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.carry);
    try testing.expectEqual(false, ctx.c.statusRegister.zero);

    // Test negative flag when result has bit 7 set
    ctx = setup();
    ctx.c.yRegister = 0x40;
    ctx.b.write(0, cpyImmediate);
    ctx.b.write(1, 0xC0);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);
    try testing.expectEqual(false, ctx.c.statusRegister.carry);

    // Verify Y register unchanged
    ctx = setup();
    ctx.c.yRegister = 0x42;
    ctx.b.write(0, cpyImmediate);
    ctx.b.write(1, 0x40);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x42, ctx.c.yRegister);
}

test "CPU DEC" {
    var ctx = setup();
    const decZp = cpu.Cpu.getOpcode("DEC", .zero_page);

    // Basic decrement
    ctx.b.write(0, decZp);
    ctx.b.write(1, 0x50);
    ctx.b.write(0x50, 0x42);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x41, ctx.b.read(0x50));

    // Zero flag when result is zero
    ctx = setup();
    ctx.b.write(0, decZp);
    ctx.b.write(1, 0x50);
    ctx.b.write(0x50, 0x01);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);
    try testing.expectEqual(0x00, ctx.b.read(0x50));

    // Negative flag when bit 7 is set
    ctx = setup();
    ctx.b.write(0, decZp);
    ctx.b.write(1, 0x50);
    ctx.b.write(0x50, 0x00);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);
    try testing.expectEqual(0xFF, ctx.b.read(0x50));

    // Wrapping from 0 to FF
    ctx = setup();
    ctx.b.write(0, decZp);
    ctx.b.write(1, 0x50);
    ctx.b.write(0x50, 0x00);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0xFF, ctx.b.read(0x50));
}

test "CPU DEX" {
    var ctx = setup();
    const dex = cpu.Cpu.getOpcode("DEX", .implied);

    // Basic decrement
    ctx.c.xRegister = 0x42;
    ctx.b.write(0, dex);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x41, ctx.c.xRegister);

    // Zero flag when result is zero
    ctx = setup();
    ctx.c.xRegister = 0x01;
    ctx.b.write(0, dex);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);
    try testing.expectEqual(0x00, ctx.c.xRegister);

    // Negative flag when bit 7 is set
    ctx = setup();
    ctx.c.xRegister = 0x00;
    ctx.b.write(0, dex);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);
    try testing.expectEqual(0xFF, ctx.c.xRegister);

    // Wrapping from 0 to FF
    ctx = setup();
    ctx.c.xRegister = 0x00;
    ctx.b.write(0, dex);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0xFF, ctx.c.xRegister);
}

test "CPU DEY" {
    var ctx = setup();
    const dey = cpu.Cpu.getOpcode("DEY", .implied);

    // Basic decrement
    ctx.c.yRegister = 0x42;
    ctx.b.write(0, dey);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x41, ctx.c.yRegister);

    // Zero flag when result is zero
    ctx = setup();
    ctx.c.yRegister = 0x01;
    ctx.b.write(0, dey);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);
    try testing.expectEqual(0x00, ctx.c.yRegister);

    // Negative flag when bit 7 is set
    ctx = setup();
    ctx.c.yRegister = 0x00;
    ctx.b.write(0, dey);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);
    try testing.expectEqual(0xFF, ctx.c.yRegister);

    // Wrapping from 0 to FF 
    ctx = setup();
    ctx.c.yRegister = 0x00;
    ctx.b.write(0, dey);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0xFF, ctx.c.yRegister);
}

test "CPU EOR" {
    var ctx = setup();
    const eorImmediate = cpu.Cpu.getOpcode("EOR", .immediate);

    // Basic exclusive OR operation
    ctx.c.accumulator = 0b10101010;
    ctx.b.write(0, eorImmediate);
    ctx.b.write(1, 0b11110000);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0b01011010, ctx.c.accumulator);

    // Zero flag when result is zero
    ctx = setup();
    ctx.c.accumulator = 0b11110000;
    ctx.b.write(0, eorImmediate);
    ctx.b.write(1, 0b11110000);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);
    try testing.expectEqual(0x00, ctx.c.accumulator);

    // Negative flag when bit 7 is set
    ctx = setup();
    ctx.c.accumulator = 0b00110011;
    ctx.b.write(0, eorImmediate);
    ctx.b.write(1, 0b10101010);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);
    try testing.expectEqual(0b10011001, ctx.c.accumulator);
}

test "CPU INC" {
    var ctx = setup();
    const incZp = cpu.Cpu.getOpcode("INC", .zero_page);

    // Basic increment
    ctx.b.write(0, incZp);  // INC zero page 
    ctx.b.write(1, 0x50);   // Zero page address
    ctx.b.write(0x50, 0x42); // Value to increment
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x43, ctx.b.read(0x50));

    // Zero flag when result is zero
    ctx = setup();
    ctx.b.write(0, incZp);
    ctx.b.write(1, 0x50);
    ctx.b.write(0x50, 0xFF); // Will increment to zero
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);
    try testing.expectEqual(0x00, ctx.b.read(0x50));

    // Negative flag when bit 7 is set 
    ctx = setup();
    ctx.b.write(0, incZp);
    ctx.b.write(1, 0x50);
    ctx.b.write(0x50, 0x7F); // Will increment to 0x80
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);
    try testing.expectEqual(0x80, ctx.b.read(0x50));

    // Wrapping from FF to 00
    ctx = setup();
    ctx.b.write(0, incZp);
    ctx.b.write(1, 0x50);
    ctx.b.write(0x50, 0xFF);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x00, ctx.b.read(0x50));
}

test "CPU INX" {
    var ctx = setup();
    const inx = cpu.Cpu.getOpcode("INX", .implied);

    // Basic increment
    ctx.c.xRegister = 0x42;
    ctx.b.write(0, inx);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x43, ctx.c.xRegister);

    // Zero flag when result is zero
    ctx = setup();
    ctx.c.xRegister = 0xFF;
    ctx.b.write(0, inx);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);
    try testing.expectEqual(0x00, ctx.c.xRegister);

    // Negative flag when bit 7 is set
    ctx = setup();
    ctx.c.xRegister = 0x7F;
    ctx.b.write(0, inx);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);
    try testing.expectEqual(0x80, ctx.c.xRegister);

    // Wrapping from FF to 00
    ctx = setup();
    ctx.c.xRegister = 0xFF;
    ctx.b.write(0, inx);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x00, ctx.c.xRegister);
}

test "CPU INY" {
    var ctx = setup();
    const iny = cpu.Cpu.getOpcode("INY", .implied);

    // Basic increment
    ctx.c.yRegister = 0x42;
    ctx.b.write(0, iny);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x43, ctx.c.yRegister);

    // Zero flag when result is zero
    ctx = setup();
    ctx.c.yRegister = 0xFF;
    ctx.b.write(0, iny);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);
    try testing.expectEqual(0x00, ctx.c.yRegister);

    // Negative flag when bit 7 is set
    ctx = setup();
    ctx.c.yRegister = 0x7F;
    ctx.b.write(0, iny);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);
    try testing.expectEqual(0x80, ctx.c.yRegister);

    // Wrapping from FF to 00
    ctx = setup();
    ctx.c.yRegister = 0xFF;
    ctx.b.write(0, iny);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x00, ctx.c.yRegister);
}

test "CPU JMP" {
    var ctx = setup();
    const jmpAbs = cpu.Cpu.getOpcode("JMP", .absolute);
    const jmpInd = cpu.Cpu.getOpcode("JMP", .indirect_bug);

    // Absolute addressing
    ctx.b.write(0, jmpAbs);
    ctx.b.write(1, 0x34);
    ctx.b.write(2, 0x12);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x1234, ctx.c.programCounter);

    // Indirect addressing
    ctx = setup();
    ctx.b.write(0, jmpInd);
    ctx.b.write(1, 0x20);
    ctx.b.write(2, 0x00);
    ctx.b.write(0x20, 0x34);
    ctx.b.write(0x21, 0x12);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x1234, ctx.c.programCounter);

    // Indirect page boundary bug
    ctx = setup();
    ctx.b.write(0, jmpInd);
    ctx.b.write(1, 0xFF);
    ctx.b.write(2, 0x02);
    ctx.b.write(0x02FF, 0x34);
    ctx.b.write(0x0200, 0x12);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x1234, ctx.c.programCounter);
}

test "CPU JSR" {
    var ctx = setup();
    const jsr = cpu.Cpu.getOpcode("JSR", .absolute);

    // Test program counter update
    ctx.b.write(0, jsr);
    ctx.b.write(1, 0x34);
    ctx.b.write(2, 0x12);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(@as(u16, 0x1234), ctx.c.programCounter);

    // Test return address pushed to stack
    ctx = setup();
    ctx.c.stackPointer = 0xFF;
    ctx.c.programCounter = 0x1234;
    ctx.b.write(0x1234, jsr);
    ctx.b.write(0x1235, 0x34);
    ctx.b.write(0x1236, 0x12);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(@as(u8, 0x12), ctx.b.read(0x01FF));
    try testing.expectEqual(@as(u8, 0x36), ctx.b.read(0x01FE));

    // Test stack pointer decremented
    ctx = setup();
    ctx.c.stackPointer = 0xFF;
    ctx.b.write(0, jsr);
    ctx.b.write(1, 0x34);
    ctx.b.write(2, 0x12);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(@as(u8, 0xFD), ctx.c.stackPointer);
}



test "CPU LDA" {
    var ctx = setup();
    const ldaImmediate = cpu.Cpu.getOpcode("LDA", .immediate);

    // Basic load
    ctx.b.write(0, ldaImmediate);
    ctx.b.write(1, 0x42);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x42, ctx.c.accumulator);

    // Zero flag when loading zero
    ctx = setup();
    ctx.b.write(0, ldaImmediate);
    ctx.b.write(1, 0x00);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);
    try testing.expectEqual(0x00, ctx.c.accumulator);

    // Negative flag when bit 7 is set
    ctx = setup();
    ctx.b.write(0, ldaImmediate);
    ctx.b.write(1, 0x80);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);
    try testing.expectEqual(0x80, ctx.c.accumulator);

    // Flags cleared when loading positive non-zero value
    ctx = setup();
    ctx.c.statusRegister.zero = true;
    ctx.c.statusRegister.negative = true;
    ctx.b.write(0, ldaImmediate);
    ctx.b.write(1, 0x42);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(false, ctx.c.statusRegister.zero);
    try testing.expectEqual(false, ctx.c.statusRegister.negative);
}

test "CPU LDX" {
    var ctx = setup();
    const ldxImmediate = cpu.Cpu.getOpcode("LDX", .immediate);

    // Basic load
    ctx.b.write(0, ldxImmediate);
    ctx.b.write(1, 0x42);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x42, ctx.c.xRegister);

    // Zero flag when loading zero
    ctx = setup();
    ctx.b.write(0, ldxImmediate);
    ctx.b.write(1, 0x00);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);
    try testing.expectEqual(0x00, ctx.c.xRegister);

    // Negative flag when bit 7 is set
    ctx = setup();
    ctx.b.write(0, ldxImmediate);
    ctx.b.write(1, 0x80);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);
    try testing.expectEqual(0x80, ctx.c.xRegister);

    // Flags cleared when loading positive non-zero value 
    ctx = setup();
    ctx.c.statusRegister.zero = true;
    ctx.c.statusRegister.negative = true;
    ctx.b.write(0, ldxImmediate);
    ctx.b.write(1, 0x42);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(false, ctx.c.statusRegister.zero);
    try testing.expectEqual(false, ctx.c.statusRegister.negative);
}

test "CPU LDY" {
    var ctx = setup();
    const ldyImmediate = cpu.Cpu.getOpcode("LDY", .immediate);

    // Basic load
    ctx.b.write(0, ldyImmediate);
    ctx.b.write(1, 0x42);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x42, ctx.c.yRegister);

    // Zero flag when loading zero
    ctx = setup();
    ctx.b.write(0, ldyImmediate);
    ctx.b.write(1, 0x00);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);
    try testing.expectEqual(0x00, ctx.c.yRegister);

    // Negative flag when bit 7 is set
    ctx = setup();
    ctx.b.write(0, ldyImmediate);
    ctx.b.write(1, 0x80);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);
    try testing.expectEqual(0x80, ctx.c.yRegister);

    // Flags cleared when loading positive non-zero value
    ctx = setup();
    ctx.c.statusRegister.zero = true;
    ctx.c.statusRegister.negative = true;
    ctx.b.write(0, ldyImmediate);
    ctx.b.write(1, 0x42);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(false, ctx.c.statusRegister.zero);
    try testing.expectEqual(false, ctx.c.statusRegister.negative);
}

test "CPU LSR" {
    var ctx = setup();
    const lsrAcc = cpu.Cpu.getOpcode("LSR", .accumulator);

    // Basic shift accumulator
    ctx.c.accumulator = 0b10101010;
    ctx.b.write(0, lsrAcc);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0b01010101, ctx.c.accumulator);

    // Carry flag gets old bit 0
    ctx = setup();
    ctx.c.accumulator = 0b10101011;
    ctx.b.write(0, lsrAcc);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.carry);
    try testing.expectEqual(0b01010101, ctx.c.accumulator);

    // Zero flag when result is zero, carry from bit 0
    ctx = setup();
    ctx.c.accumulator = 0b00000001;
    ctx.b.write(0, lsrAcc);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);
    try testing.expectEqual(true, ctx.c.statusRegister.carry);
    try testing.expectEqual(0b00000000, ctx.c.accumulator);

    // Carry clear when bit 0 is clear (even if bit 7 set)
    ctx = setup();
    ctx.c.accumulator = 0b10000000;
    ctx.b.write(0, lsrAcc);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(false, ctx.c.statusRegister.carry);
    try testing.expectEqual(0b01000000, ctx.c.accumulator);

    // Negative flag is always clear
    ctx = setup();
    ctx.c.accumulator = 0b11111111;
    ctx.b.write(0, lsrAcc);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(false, ctx.c.statusRegister.negative);
    try testing.expectEqual(0b01111111, ctx.c.accumulator);
}

test "CPU NOP" {
    var ctx = setup();
    const nop = cpu.Cpu.getOpcode("NOP", .implied);

    // Test NOP advances program counter
    const oldPc = ctx.c.programCounter;
    ctx.b.write(0, nop);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(oldPc + 1, ctx.c.programCounter);

    // Test preserves all registers
    ctx = setup();
    ctx.c.accumulator = 0x42;
    ctx.c.xRegister = 0x43;
    ctx.c.yRegister = 0x44;
    ctx.b.write(0, nop);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x42, ctx.c.accumulator);
    try testing.expectEqual(0x43, ctx.c.xRegister);
    try testing.expectEqual(0x44, ctx.c.yRegister);

    // Test preserves all flags
    ctx = setup();
    ctx.c.statusRegister.zero = true;
    ctx.c.statusRegister.negative = true;
    ctx.c.statusRegister.carry = true;
    ctx.c.statusRegister.overflow = true;
    ctx.c.statusRegister.decimal = true;
    ctx.c.statusRegister.interrupt_disable = true;
    ctx.b.write(0, nop);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);
    try testing.expectEqual(true, ctx.c.statusRegister.carry);
    try testing.expectEqual(true, ctx.c.statusRegister.overflow);
    try testing.expectEqual(true, ctx.c.statusRegister.decimal);
    try testing.expectEqual(true, ctx.c.statusRegister.interrupt_disable);
}

test "CPU ORA" {
    var ctx = setup();
    const oraImmediate = cpu.Cpu.getOpcode("ORA", .immediate);

    // Basic inclusive or operation
    ctx.c.accumulator = 0b10101010;
    ctx.b.write(0, oraImmediate);
    ctx.b.write(1, 0b11110000);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0b11111010, ctx.c.accumulator);

    // Zero flag when result is zero
    ctx = setup();
    ctx.c.accumulator = 0b00000000;
    ctx.b.write(0, oraImmediate);
    ctx.b.write(1, 0b00000000);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);
    try testing.expectEqual(0x00, ctx.c.accumulator);

    // Negative flag when bit 7 is set
    ctx = setup();
    ctx.c.accumulator = 0b00110011;
    ctx.b.write(0, oraImmediate);
    ctx.b.write(1, 0b10000000);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);
    try testing.expectEqual(0b10110011, ctx.c.accumulator);
}

test "CPU PHA" {
    var ctx = setup();
    const pha = cpu.Cpu.getOpcode("PHA", .implied_bus);

    // Basic push
    ctx.c.accumulator = 0x42;
    ctx.c.stackPointer = 0xFF;
    ctx.b.write(0, pha);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x42, ctx.b.read(0x01FF));

    // Decrements stack pointer
    ctx = setup();
    ctx.c.stackPointer = 0xFF;
    ctx.b.write(0, pha);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0xFE, ctx.c.stackPointer);

    // Preserves accumulator
    ctx = setup();
    ctx.c.accumulator = 0x42;
    ctx.b.write(0, pha);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x42, ctx.c.accumulator);

    // Doesn't affect flags
    ctx = setup();
    ctx.c.statusRegister.zero = true;
    ctx.c.statusRegister.negative = true;
    ctx.c.statusRegister.carry = true;
    ctx.c.statusRegister.overflow = true;
    ctx.b.write(0, pha);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);
    try testing.expectEqual(true, ctx.c.statusRegister.carry);
    try testing.expectEqual(true, ctx.c.statusRegister.overflow);
}

test "CPU PHP" {
    var ctx = setup();
    const php = cpu.Cpu.getOpcode("PHP", .implied_bus);

    // Basic push flags 
    ctx.c.stackPointer = 0xFF;
    ctx.c.statusRegister.zero = true;
    ctx.c.statusRegister.negative = true;
    ctx.c.statusRegister.carry = true;
    ctx.c.statusRegister.overflow = true;
    ctx.b.write(0, php);
    ctx.c.step(&ctx.b);

    const pushedFlags = ctx.b.read(0x01FF);
    try testing.expectEqual(true, (pushedFlags & 0x80) != 0); // Negative
    try testing.expectEqual(true, (pushedFlags & 0x40) != 0); // Overflow
    try testing.expectEqual(true, (pushedFlags & 0x02) != 0); // Zero
    try testing.expectEqual(true, (pushedFlags & 0x01) != 0); // Carry
    try testing.expectEqual(true, (pushedFlags & 0x10) != 0); // Break
    try testing.expectEqual(true, (pushedFlags & 0x20) != 0); // Unused

    // Decrements stack pointer
    ctx = setup();
    ctx.c.stackPointer = 0xFF;
    ctx.b.write(0, php);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0xFE, ctx.c.stackPointer);

    // Preserves flags
    ctx = setup();
    ctx.c.statusRegister.zero = true;
    ctx.c.statusRegister.negative = true;
    ctx.c.statusRegister.carry = true;
    ctx.c.statusRegister.overflow = true;
    ctx.b.write(0, php);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);
    try testing.expectEqual(true, ctx.c.statusRegister.carry);
    try testing.expectEqual(true, ctx.c.statusRegister.overflow);
}

test "CPU PLA" {
    var ctx = setup();
    const pla = cpu.Cpu.getOpcode("PLA", .implied_bus);

    // Basic pull
    ctx.c.stackPointer = 0xFE;
    ctx.b.write(0x01FF, 0x42);
    ctx.b.write(0, pla);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x42, ctx.c.accumulator);

    // Increments stack pointer
    ctx = setup();
    ctx.c.stackPointer = 0xFE;
    ctx.b.write(0, pla);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0xFF, ctx.c.stackPointer);

    // Zero flag when pulling zero
    ctx = setup();
    ctx.c.stackPointer = 0xFE;
    ctx.b.write(0x01FF, 0x00);
    ctx.b.write(0, pla);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);
    try testing.expectEqual(0x00, ctx.c.accumulator);

    // Negative flag when pulling negative value
    ctx = setup();
    ctx.c.stackPointer = 0xFE;
    ctx.b.write(0x01FF, 0x80);
    ctx.b.write(0, pla);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);
    try testing.expectEqual(0x80, ctx.c.accumulator);
}


test "CPU PLP" {
    var ctx = setup();
    const plp = cpu.Cpu.getOpcode("PLP", .implied_bus);

    // Basic pull flags
    ctx.c.stackPointer = 0xFE;
    ctx.b.write(0x01FF, 0b11000011); // N=1, V=1, Z=1, C=1
    ctx.b.write(0, plp);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);
    try testing.expectEqual(true, ctx.c.statusRegister.overflow);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);
    try testing.expectEqual(true, ctx.c.statusRegister.carry);

    // Increments stack pointer
    ctx = setup();
    ctx.c.stackPointer = 0xFE;
    ctx.b.write(0, plp);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0xFF, ctx.c.stackPointer);

    // Ignores break flag from pulled value
    ctx = setup();
    ctx.c.stackPointer = 0xFE;
    ctx.c.statusRegister.break_cmd = false;
    ctx.b.write(0x01FF, 0b00010000); // B=1 in pulled value
    ctx.b.write(0, plp);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(false, ctx.c.statusRegister.break_cmd);

    // Unused flag stays true
    ctx = setup();
    ctx.c.stackPointer = 0xFE;
    ctx.b.write(0x01FF, 0x00); // All zeros including unused
    ctx.b.write(0, plp);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.unused);

    // Clears flags when pulling zeros
    ctx = setup();
    ctx.c.stackPointer = 0xFE;
    ctx.c.statusRegister.negative = true;
    ctx.c.statusRegister.overflow = true;
    ctx.c.statusRegister.zero = true;
    ctx.c.statusRegister.carry = true;
    ctx.b.write(0x01FF, 0x00);
    ctx.b.write(0, plp);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(false, ctx.c.statusRegister.negative);
    try testing.expectEqual(false, ctx.c.statusRegister.overflow);
    try testing.expectEqual(false, ctx.c.statusRegister.zero);
    try testing.expectEqual(false, ctx.c.statusRegister.carry);
}

test "CPU ROL" {
    var ctx = setup();
    const rol_a = cpu.Cpu.getOpcode("ROL", .accumulator);

    // Basic rotate left - bit 7 goes to carry
    ctx.c.accumulator = 0b10000000;
    ctx.c.statusRegister.carry = false;
    ctx.b.write(0, rol_a);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0b00000000, ctx.c.accumulator);
    try testing.expectEqual(true, ctx.c.statusRegister.carry);

    // Carry rotates into bit 0
    ctx = setup();
    ctx.c.accumulator = 0b00000000;
    ctx.c.statusRegister.carry = true;
    ctx.b.write(0, rol_a);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0b00000001, ctx.c.accumulator);
    try testing.expectEqual(false, ctx.c.statusRegister.carry);

    // Full rotation with carry in and out
    ctx = setup();
    ctx.c.accumulator = 0b10000001;
    ctx.c.statusRegister.carry = true;
    ctx.b.write(0, rol_a);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0b00000011, ctx.c.accumulator);
    try testing.expectEqual(true, ctx.c.statusRegister.carry);

    // Sets zero flag
    ctx = setup();
    ctx.c.accumulator = 0b10000000;
    ctx.c.statusRegister.carry = false;
    ctx.b.write(0, rol_a);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);

    // Sets negative flag
    ctx = setup();
    ctx.c.accumulator = 0b01000000;
    ctx.c.statusRegister.carry = false;
    ctx.b.write(0, rol_a);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);
}

test "CPU ROR" {
    var ctx = setup();
    const ror_a = cpu.Cpu.getOpcode("ROR", .accumulator);

    // Basic rotate right - bit 0 goes to carry
    ctx.c.accumulator = 0b00000001;
    ctx.c.statusRegister.carry = false;
    ctx.b.write(0, ror_a);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0b00000000, ctx.c.accumulator);
    try testing.expectEqual(true, ctx.c.statusRegister.carry);

    // Carry rotates into bit 7
    ctx = setup();
    ctx.c.accumulator = 0b00000000;
    ctx.c.statusRegister.carry = true;
    ctx.b.write(0, ror_a);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0b10000000, ctx.c.accumulator);
    try testing.expectEqual(false, ctx.c.statusRegister.carry);

    // Full rotation with carry in and out
    ctx = setup();
    ctx.c.accumulator = 0b10000001;
    ctx.c.statusRegister.carry = true;
    ctx.b.write(0, ror_a);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0b11000000, ctx.c.accumulator);
    try testing.expectEqual(true, ctx.c.statusRegister.carry);

    // Sets zero flag
    ctx = setup();
    ctx.c.accumulator = 0b00000001;
    ctx.c.statusRegister.carry = false;
    ctx.b.write(0, ror_a);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);

    // Sets negative flag (when carry rotates into bit 7)
    ctx = setup();
    ctx.c.accumulator = 0b00000000;
    ctx.c.statusRegister.carry = true;
    ctx.b.write(0, ror_a);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);
}

test "CPU RTI" {
    var ctx = setup();
    const rti = cpu.Cpu.getOpcode("RTI", .implied_bus);

    // Returns to correct address and restores flags
    ctx.c.stackPointer = 0xFC;
    ctx.b.write(0x01FD, 0b11000011); // Status: N=1, V=1, Z=1, C=1
    ctx.b.write(0x01FE, 0x34); // PC low byte
    ctx.b.write(0x01FF, 0x12); // PC high byte
    ctx.b.write(0, rti);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x1234, ctx.c.programCounter);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);
    try testing.expectEqual(true, ctx.c.statusRegister.overflow);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);
    try testing.expectEqual(true, ctx.c.statusRegister.carry);
    try testing.expectEqual(0xFF, ctx.c.stackPointer);

    // Ignores break flag from pulled value
    ctx = setup();
    ctx.c.stackPointer = 0xFC;
    ctx.c.statusRegister.break_cmd = false;
    ctx.b.write(0x01FD, 0b00010000); // B=1 in pulled value
    ctx.b.write(0x01FE, 0x00);
    ctx.b.write(0x01FF, 0x00);
    ctx.b.write(0, rti);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(false, ctx.c.statusRegister.break_cmd);
}

test "CPU RTS" {
    var ctx = setup();
    const rts = cpu.Cpu.getOpcode("RTS", .implied_bus);

    // Returns to address + 1
    ctx.c.stackPointer = 0xFD;
    ctx.b.write(0x01FE, 0x33); // PC low byte (will become 0x1234 after +1)
    ctx.b.write(0x01FF, 0x12); // PC high byte
    ctx.b.write(0, rts);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x1234, ctx.c.programCounter);
    try testing.expectEqual(0xFF, ctx.c.stackPointer);

    // Doesn't affect flags
    ctx = setup();
    ctx.c.stackPointer = 0xFD;
    ctx.c.statusRegister.negative = true;
    ctx.c.statusRegister.overflow = true;
    ctx.c.statusRegister.zero = true;
    ctx.c.statusRegister.carry = true;
    ctx.b.write(0x01FE, 0x00);
    ctx.b.write(0x01FF, 0x00);
    ctx.b.write(0, rts);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);
    try testing.expectEqual(true, ctx.c.statusRegister.overflow);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);
    try testing.expectEqual(true, ctx.c.statusRegister.carry);
}

test "CPU SBC" {
    var ctx = setup();
    const sbc_imm = cpu.Cpu.getOpcode("SBC", .immediate);

    // Basic subtraction: 0x50 - 0x10 - (1-1) = 0x40
    ctx.c.accumulator = 0x50;
    ctx.c.statusRegister.carry = true; // No borrow
    ctx.b.write(0, sbc_imm);
    ctx.b.write(1, 0x10);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x40, ctx.c.accumulator);
    try testing.expectEqual(true, ctx.c.statusRegister.carry); // No borrow occurred

    // Subtraction with borrow: 0x50 - 0x10 - 1 = 0x3F
    ctx = setup();
    ctx.c.accumulator = 0x50;
    ctx.c.statusRegister.carry = false; // Borrow
    ctx.b.write(0, sbc_imm);
    ctx.b.write(1, 0x10);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x3F, ctx.c.accumulator);
    try testing.expectEqual(true, ctx.c.statusRegister.carry);

    // Sets zero flag: 0x10 - 0x10 = 0x00
    ctx = setup();
    ctx.c.accumulator = 0x10;
    ctx.c.statusRegister.carry = true;
    ctx.b.write(0, sbc_imm);
    ctx.b.write(1, 0x10);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x00, ctx.c.accumulator);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);

    // Carry set when A == M (no borrow occurs)
    ctx = setup();
    ctx.c.accumulator = 0x50;
    ctx.c.statusRegister.carry = true;
    ctx.b.write(0, sbc_imm);
    ctx.b.write(1, 0x50);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x00, ctx.c.accumulator);
    try testing.expectEqual(true, ctx.c.statusRegister.carry); // No borrow when A == M

    // Clears carry (sets borrow): 0x10 - 0x20 = 0xF0 (underflow)
    ctx = setup();
    ctx.c.accumulator = 0x10;
    ctx.c.statusRegister.carry = true;
    ctx.b.write(0, sbc_imm);
    ctx.b.write(1, 0x20);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0xF0, ctx.c.accumulator);
    try testing.expectEqual(false, ctx.c.statusRegister.carry); // Borrow occurred

    // Sets negative flag
    ctx = setup();
    ctx.c.accumulator = 0x00;
    ctx.c.statusRegister.carry = true;
    ctx.b.write(0, sbc_imm);
    ctx.b.write(1, 0x01);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0xFF, ctx.c.accumulator);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);

    // Sets overflow flag: 0x80 - 0x01 = 0x7F (negative - positive = positive)
    ctx = setup();
    ctx.c.accumulator = 0x80;
    ctx.c.statusRegister.carry = true;
    ctx.b.write(0, sbc_imm);
    ctx.b.write(1, 0x01);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x7F, ctx.c.accumulator);
    try testing.expectEqual(true, ctx.c.statusRegister.overflow);

    // Sets overflow flag: 0x7F - 0xFF = 0x80 (positive - negative = negative)
    ctx = setup();
    ctx.c.accumulator = 0x7F;
    ctx.c.statusRegister.carry = true;
    ctx.b.write(0, sbc_imm);
    ctx.b.write(1, 0xFF); // -1 in signed
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x80, ctx.c.accumulator);
    try testing.expectEqual(true, ctx.c.statusRegister.overflow);
}

test "CPU SEC" {
    var ctx = setup();
    const sec = cpu.Cpu.getOpcode("SEC", .implied);

    // Sets carry flag
    ctx.c.statusRegister.carry = false;
    ctx.b.write(0, sec);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.carry);

    // Carry already set stays set
    ctx = setup();
    ctx.c.statusRegister.carry = true;
    ctx.b.write(0, sec);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.carry);

    // Doesn't affect other flags
    ctx = setup();
    ctx.c.statusRegister.negative = true;
    ctx.c.statusRegister.overflow = true;
    ctx.c.statusRegister.zero = true;
    ctx.c.statusRegister.carry = false;
    ctx.b.write(0, sec);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);
    try testing.expectEqual(true, ctx.c.statusRegister.overflow);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);
}

test "CPU SED" {
    var ctx = setup();
    const sed = cpu.Cpu.getOpcode("SED", .implied);

    // Sets decimal flag when clear
    ctx.c.statusRegister.decimal = false;
    ctx.b.write(0, sed);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.decimal);

    // Decimal already set stays set
    ctx = setup();
    ctx.c.statusRegister.decimal = true;
    ctx.b.write(0, sed);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.decimal);

    // Doesn't affect other flags
    ctx = setup();
    ctx.c.statusRegister.zero = true;
    ctx.c.statusRegister.overflow = true;
    ctx.c.statusRegister.negative = true;
    ctx.c.statusRegister.carry = true;
    ctx.c.statusRegister.interrupt_disable = true;
    ctx.c.statusRegister.decimal = false;
    ctx.b.write(0, sed);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);
    try testing.expectEqual(true, ctx.c.statusRegister.overflow);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);
    try testing.expectEqual(true, ctx.c.statusRegister.carry);
    try testing.expectEqual(true, ctx.c.statusRegister.interrupt_disable);
}

test "CPU SEI" {
    var ctx = setup();
    const sei = cpu.Cpu.getOpcode("SEI", .implied);

    // Sets interrupt flag when clear
    ctx.c.statusRegister.interrupt_disable = false;
    ctx.b.write(0, sei);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.interrupt_disable);

    // Interrupt already set stays set
    ctx = setup();
    ctx.c.statusRegister.interrupt_disable = true;
    ctx.b.write(0, sei);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.interrupt_disable);

    // Doesn't affect other flags
    ctx = setup();
    ctx.c.statusRegister.zero = true;
    ctx.c.statusRegister.overflow = true;
    ctx.c.statusRegister.negative = true;
    ctx.c.statusRegister.carry = true;
    ctx.c.statusRegister.decimal = true;
    ctx.c.statusRegister.interrupt_disable = false;
    ctx.b.write(0, sei);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);
    try testing.expectEqual(true, ctx.c.statusRegister.overflow);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);
    try testing.expectEqual(true, ctx.c.statusRegister.carry);
    try testing.expectEqual(true, ctx.c.statusRegister.decimal);
}

test "CPU STA" {
    var ctx = setup();
    const sta_zp = cpu.Cpu.getOpcode("STA", .zero_page);

    // Basic store
    ctx.c.accumulator = 0x42;
    ctx.b.write(0, sta_zp);
    ctx.b.write(1, 0x50);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x42, ctx.b.read(0x50));

    // Preserves accumulator value
    ctx = setup();
    ctx.c.accumulator = 0x42;
    ctx.b.write(0, sta_zp);
    ctx.b.write(1, 0x50);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x42, ctx.c.accumulator);

    // Doesn't affect any flags
    ctx = setup();
    ctx.c.accumulator = 0x80; // Negative value
    ctx.c.statusRegister.zero = true;
    ctx.c.statusRegister.negative = false;
    ctx.c.statusRegister.carry = true;
    ctx.c.statusRegister.overflow = true;
    ctx.b.write(0, sta_zp);
    ctx.b.write(1, 0x50);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);
    try testing.expectEqual(false, ctx.c.statusRegister.negative);
    try testing.expectEqual(true, ctx.c.statusRegister.carry);
    try testing.expectEqual(true, ctx.c.statusRegister.overflow);
}

test "CPU STX" {
    var ctx = setup();
    const stx_zp = cpu.Cpu.getOpcode("STX", .zero_page);

    // Basic store
    ctx.c.xRegister = 0x42;
    ctx.b.write(0, stx_zp);
    ctx.b.write(1, 0x50);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x42, ctx.b.read(0x50));

    // Preserves X register value
    ctx = setup();
    ctx.c.xRegister = 0x42;
    ctx.b.write(0, stx_zp);
    ctx.b.write(1, 0x50);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x42, ctx.c.xRegister);

    // Doesn't affect any flags
    ctx = setup();
    ctx.c.xRegister = 0x80; // Negative value
    ctx.c.statusRegister.zero = true;
    ctx.c.statusRegister.negative = false;
    ctx.c.statusRegister.carry = true;
    ctx.c.statusRegister.overflow = true;
    ctx.b.write(0, stx_zp);
    ctx.b.write(1, 0x50);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);
    try testing.expectEqual(false, ctx.c.statusRegister.negative);
    try testing.expectEqual(true, ctx.c.statusRegister.carry);
    try testing.expectEqual(true, ctx.c.statusRegister.overflow);
}

test "CPU STY" {
    var ctx = setup();
    const sty_zp = cpu.Cpu.getOpcode("STY", .zero_page);

    // Basic store
    ctx.c.yRegister = 0x42;
    ctx.b.write(0, sty_zp);
    ctx.b.write(1, 0x50);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x42, ctx.b.read(0x50));

    // Preserves Y register value
    ctx = setup();
    ctx.c.yRegister = 0x42;
    ctx.b.write(0, sty_zp);
    ctx.b.write(1, 0x50);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x42, ctx.c.yRegister);

    // Doesn't affect any flags
    ctx = setup();
    ctx.c.yRegister = 0x80; // Negative value
    ctx.c.statusRegister.zero = true;
    ctx.c.statusRegister.negative = false;
    ctx.c.statusRegister.carry = true;
    ctx.c.statusRegister.overflow = true;
    ctx.b.write(0, sty_zp);
    ctx.b.write(1, 0x50);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);
    try testing.expectEqual(false, ctx.c.statusRegister.negative);
    try testing.expectEqual(true, ctx.c.statusRegister.carry);
    try testing.expectEqual(true, ctx.c.statusRegister.overflow);
}

test "CPU TAX" {
    var ctx = setup();
    const tax = cpu.Cpu.getOpcode("TAX", .implied);

    // Basic transfer
    ctx.c.accumulator = 0x42;
    ctx.b.write(0, tax);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x42, ctx.c.xRegister);

    // Preserves accumulator
    ctx = setup();
    ctx.c.accumulator = 0x42;
    ctx.b.write(0, tax);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x42, ctx.c.accumulator);

    // Zero flag when transferring zero
    ctx = setup();
    ctx.c.accumulator = 0x00;
    ctx.b.write(0, tax);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);
    try testing.expectEqual(0x00, ctx.c.xRegister);

    // Negative flag when transferring negative value
    ctx = setup();
    ctx.c.accumulator = 0x80;
    ctx.b.write(0, tax);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);
    try testing.expectEqual(0x80, ctx.c.xRegister);
}

test "CPU TAY" {
    var ctx = setup();
    const tay = cpu.Cpu.getOpcode("TAY", .implied);

    // Basic transfer
    ctx.c.accumulator = 0x42;
    ctx.b.write(0, tay);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x42, ctx.c.yRegister);

    // Preserves accumulator
    ctx = setup();
    ctx.c.accumulator = 0x42;
    ctx.b.write(0, tay);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x42, ctx.c.accumulator);

    // Zero flag when transferring zero
    ctx = setup();
    ctx.c.accumulator = 0x00;
    ctx.b.write(0, tay);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);
    try testing.expectEqual(0x00, ctx.c.yRegister);

    // Negative flag when transferring negative value
    ctx = setup();
    ctx.c.accumulator = 0x80;
    ctx.b.write(0, tay);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);
    try testing.expectEqual(0x80, ctx.c.yRegister);
}

test "CPU TSX" {
    var ctx = setup();
    const tsx = cpu.Cpu.getOpcode("TSX", .implied);

    // Basic transfer
    ctx.c.stackPointer = 0x42;
    ctx.b.write(0, tsx);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x42, ctx.c.xRegister);

    // Preserves stack pointer
    ctx = setup();
    ctx.c.stackPointer = 0x42;
    ctx.b.write(0, tsx);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x42, ctx.c.stackPointer);

    // Zero flag when transferring zero
    ctx = setup();
    ctx.c.stackPointer = 0x00;
    ctx.b.write(0, tsx);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);
    try testing.expectEqual(0x00, ctx.c.xRegister);

    // Negative flag when transferring negative value
    ctx = setup();
    ctx.c.stackPointer = 0x80;
    ctx.b.write(0, tsx);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);
    try testing.expectEqual(0x80, ctx.c.xRegister);
}

test "CPU TXA" {
    var ctx = setup();
    const txa = cpu.Cpu.getOpcode("TXA", .implied);

    // Basic transfer
    ctx.c.xRegister = 0x42;
    ctx.b.write(0, txa);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x42, ctx.c.accumulator);

    // Preserves X register
    ctx = setup();
    ctx.c.xRegister = 0x42;
    ctx.b.write(0, txa);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x42, ctx.c.xRegister);

    // Zero flag when transferring zero
    ctx = setup();
    ctx.c.xRegister = 0x00;
    ctx.b.write(0, txa);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);
    try testing.expectEqual(0x00, ctx.c.accumulator);

    // Negative flag when transferring negative value
    ctx = setup();
    ctx.c.xRegister = 0x80;
    ctx.b.write(0, txa);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);
    try testing.expectEqual(0x80, ctx.c.accumulator);
}

test "CPU TXS" {
    var ctx = setup();
    const txs = cpu.Cpu.getOpcode("TXS", .implied);

    // Basic transfer
    ctx.c.xRegister = 0x42;
    ctx.b.write(0, txs);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x42, ctx.c.stackPointer);

    // Preserves X register
    ctx = setup();
    ctx.c.xRegister = 0x42;
    ctx.b.write(0, txs);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x42, ctx.c.xRegister);

    // Doesn't affect any flags
    ctx = setup();
    ctx.c.xRegister = 0x80; // Negative value
    ctx.c.statusRegister.zero = true;
    ctx.c.statusRegister.negative = true;
    ctx.c.statusRegister.carry = true;
    ctx.c.statusRegister.overflow = true;
    ctx.b.write(0, txs);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);
    try testing.expectEqual(true, ctx.c.statusRegister.carry);
    try testing.expectEqual(true, ctx.c.statusRegister.overflow);
}

test "CPU TYA" {
    var ctx = setup();
    const tya = cpu.Cpu.getOpcode("TYA", .implied);

    // Basic transfer
    ctx.c.yRegister = 0x42;
    ctx.b.write(0, tya);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x42, ctx.c.accumulator);

    // Preserves Y register
    ctx = setup();
    ctx.c.yRegister = 0x42;
    ctx.b.write(0, tya);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(0x42, ctx.c.yRegister);

    // Zero flag when transferring zero
    ctx = setup();
    ctx.c.yRegister = 0x00;
    ctx.b.write(0, tya);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.zero);
    try testing.expectEqual(0x00, ctx.c.accumulator);

    // Negative flag when transferring negative value
    ctx = setup();
    ctx.c.yRegister = 0x80;
    ctx.b.write(0, tya);
    ctx.c.step(&ctx.b);
    try testing.expectEqual(true, ctx.c.statusRegister.negative);
    try testing.expectEqual(0x80, ctx.c.accumulator);
}
