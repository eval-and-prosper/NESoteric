const std = @import("std");
const Cartridge = @import("cartridge.zig").Cartridge;
const NesHeader = @import("cartridge.zig").NesHeader;

pub const MirrorMode = enum {
    horizontal,
    vertical,
    four_screen,
    single_lower,
    single_upper,
};

// Mapper Interface
pub const Mapper = struct {
    impl: *anyopaque,
    vtable: *const VTable,

    const VTable = struct {
        read: *const fn (self: *anyopaque, address: u16) u8,
        write: *const fn (self: *anyopaque, address: u16, value: u8) void,
        ppuRead: *const fn (self: *anyopaque, address: u14) u8,
        ppuWrite: *const fn (self: *anyopaque, address: u14, value: u8) void,
        getMirrorMode: *const fn (self: *anyopaque) MirrorMode,
        deinit: *const fn (self: *anyopaque, allocator: std.mem.Allocator) void,
    };

    pub fn init(impl: anytype) Mapper {
        const T = std.meta.Child(@TypeOf(impl));
        return .{ .impl = impl, .vtable = &.{
            .read = @ptrCast(&@field(T, "read")),
            .write = @ptrCast(&@field(T, "write")),
            .ppuRead = @ptrCast(&@field(T, "ppuRead")),
            .ppuWrite = @ptrCast(&@field(T, "ppuWrite")),
            .getMirrorMode = @ptrCast(&@field(T, "getMirrorMode")),
            .deinit = @ptrCast(&@field(T, "deinit")),
        } };
    }

    pub fn read(self: *Mapper, address: u16) u8 {
        return self.vtable.read(self.impl, address);
    }

    pub fn write(self: *Mapper, address: u16, value: u8) void {
        return self.vtable.write(self.impl, address, value);
    }

    pub fn ppuRead(self: *Mapper, address: u14) u8 {
        return self.vtable.ppuRead(self.impl, address);
    }

    pub fn ppuWrite(self: *Mapper, address: u14, value: u8) void {
        return self.vtable.ppuWrite(self.impl, address, value);
    }

    pub fn getMirrorMode(self: *Mapper) MirrorMode {
        return self.vtable.getMirrorMode(self.impl);
    }

    pub fn deinit(self: *Mapper, allocator: std.mem.Allocator) void {
        return self.vtable.deinit(self.impl, allocator);
    }
};

const Mapper0 = struct {
    cart: *Cartridge,
    prgRam: [0x2000]u8 = std.mem.zeroes([0x2000]u8),
    chrRam: [0x2000]u8 = std.mem.zeroes([0x2000]u8), // used when chrRomSize == 0

    fn read(self: *Mapper0, address: u16) u8 {
        return switch (address) {
            0x6000...0x7FFF => self.prgRam[address - 0x6000],
            0x8000...0xFFFF => blk: {
                const rom_addr = if (self.cart.header.prgRomSize == 1)
                    (address - 0x8000) & 0x3FFF
                else
                    address - 0x8000;
                break :blk self.cart.prgROM.?[rom_addr];
            },
            else => 0,
        };
    }

    fn write(self: *Mapper0, address: u16, value: u8) void {
        switch (address) {
            0x6000...0x7FFF => self.prgRam[address - 0x6000] = value,
            else => {},
        }
    }

    fn ppuRead(self: *Mapper0, address: u14) u8 {
        return switch (address) {
            0x0000...0x1FFF => blk: {
                if (self.cart.chrROM) |chr| {
                    break :blk chr[address];
                } else {
                    break :blk self.chrRam[address];
                }
            },
            else => 0, // nametables/palette handled by PPU
        };
    }

    fn ppuWrite(self: *Mapper0, address: u14, value: u8) void {
        switch (address) {
            0x0000...0x1FFF => {
                // Only writable if CHR RAM (no CHR ROM)
                if (self.cart.chrROM == null) {
                    self.chrRam[address] = value;
                }
            },
            else => {},
        }
    }

    fn getMirrorMode(self: *Mapper0) MirrorMode {
        if (self.cart.header.fourScreen) return .four_screen;
        return if (self.cart.header.mirroring) .vertical else .horizontal;
    }

    fn deinit(self: *Mapper0, allocator: std.mem.Allocator) void {
        _ = self;
        _ = allocator;
    }
};

pub fn mapperFactory(allocator: std.mem.Allocator, cart: *Cartridge) !Mapper {
    const mapper_num = cart.header.mapper();
    return switch (mapper_num) {
        0 => {
            const m = try allocator.create(Mapper0);
            m.* = .{ .cart = cart };
            return Mapper.init(m);
        },
        else => @panic("Unsupported Mapper!"),
    };
}

/// Test helper that creates a Mapper0 with owned PRG-ROM and CHR-ROM for testing.
/// Useful for CPU/PPU tests that need a mapper without loading a real ROM.
pub const TestMapper = struct {
    mapper: Mapper0,
    cart: Cartridge,
    prg: [32 * 1024]u8,
    chr: [8 * 1024]u8,

    pub fn initInPlace(self: *TestMapper) void {
        self.prg = std.mem.zeroes([32 * 1024]u8);
        self.chr = std.mem.zeroes([8 * 1024]u8);
        self.cart = .{
            .header = std.mem.zeroes(NesHeader),
            .prgROM = &self.prg,
            .chrROM = &self.chr,
        };

        self.cart.header.prgRomSize = 2; // 32KB
        self.cart.header.chrRomSize = 1; // 8KB
        self.mapper = .{ .cart = &self.cart };
    }

    pub fn interface(self: *TestMapper) Mapper {
        return Mapper.init(&self.mapper);
    }

    /// Set the reset vector ($FFFC-$FFFD)
    pub fn setResetVector(self: *TestMapper, address: u16) void {
        self.prg[0x7FFC] = @truncate(address);
        self.prg[0x7FFD] = @truncate(address >> 8);
    }

    /// Set the NMI vector ($FFFA-$FFFB)
    pub fn setNmiVector(self: *TestMapper, address: u16) void {
        self.prg[0x7FFA] = @truncate(address);
        self.prg[0x7FFB] = @truncate(address >> 8);
    }

    /// Set the IRQ vector ($FFFE-$FFFF)
    pub fn setIrqVector(self: *TestMapper, address: u16) void {
        self.prg[0x7FFE] = @truncate(address);
        self.prg[0x7FFF] = @truncate(address >> 8);
    }

    /// Write a byte to PRG-ROM (for test setup)
    pub fn writeRom(self: *TestMapper, address: u16, value: u8) void {
        if (address >= 0x8000) {
            self.prg[address - 0x8000] = value;
        }
    }

    /// Write a slice to PRG-ROM starting at address
    pub fn writeRomSlice(self: *TestMapper, address: u16, data: []const u8) void {
        if (address >= 0x8000) {
            const offset = address - 0x8000;
            @memcpy(self.prg[offset..][0..data.len], data);
        }
    }

    /// Write a byte to CHR-ROM (for test setup)
    pub fn writeChr(self: *TestMapper, address: u14, value: u8) void {
        if (address < 0x2000) {
            self.chr[address] = value;
        }
    }

    /// Write a slice to CHR-ROM starting at address
    pub fn writeChrSlice(self: *TestMapper, address: u14, data: []const u8) void {
        if (address < 0x2000) {
            @memcpy(self.chr[address..][0..data.len], data);
        }
    }
};

test "Mapper0: PRG-RAM read/write" {
    var cart = Cartridge{
        .header = std.mem.zeroes(NesHeader),
        .prgROM = null,
        .chrROM = null,
    };
    var mapper = Mapper0{ .cart = &cart };

    // Write to PRG-RAM
    mapper.write(0x6000, 0xAB);
    mapper.write(0x7FFF, 0xCD);

    // Read back
    try std.testing.expectEqual(@as(u8, 0xAB), mapper.read(0x6000));
    try std.testing.expectEqual(@as(u8, 0xCD), mapper.read(0x7FFF));

    // Verify internal state
    try std.testing.expectEqual(@as(u8, 0xAB), mapper.prgRam[0]);
    try std.testing.expectEqual(@as(u8, 0xCD), mapper.prgRam[0x1FFF]);
}

test "Mapper0: 16KB PRG-ROM mirroring" {
    var prg: [16 * 1024]u8 = undefined;
    // Fill with pattern to verify reads
    for (&prg, 0..) |*byte, i| {
        byte.* = @truncate(i);
    }

    var cart = Cartridge{
        .header = std.mem.zeroes(NesHeader),
        .prgROM = prg[0..],
        .chrROM = null,
    };
    cart.header.prgRomSize = 1; // 16KB

    var mapper = Mapper0{ .cart = &cart };

    // $8000 should read ROM[0]
    try std.testing.expectEqual(@as(u8, 0x00), mapper.read(0x8000));

    // $BFFF should read ROM[0x3FFF] (end of 16KB)
    try std.testing.expectEqual(@as(u8, 0xFF), mapper.read(0xBFFF));

    // $C000 should mirror back to ROM[0]
    try std.testing.expectEqual(@as(u8, 0x00), mapper.read(0xC000));

    // $FFFF should mirror to ROM[0x3FFF]
    try std.testing.expectEqual(@as(u8, 0xFF), mapper.read(0xFFFF));

    // Verify mirroring: $8000 == $C000, $9000 == $D000, etc.
    try std.testing.expectEqual(mapper.read(0x8000), mapper.read(0xC000));
    try std.testing.expectEqual(mapper.read(0x9000), mapper.read(0xD000));
    try std.testing.expectEqual(mapper.read(0xA000), mapper.read(0xE000));
    try std.testing.expectEqual(mapper.read(0xBFFF), mapper.read(0xFFFF));
}

test "Mapper0: 32KB PRG-ROM no mirroring" {
    var prg: [32 * 1024]u8 = undefined;
    @memset(&prg, 0x00);

    // Set distinct values in each 16KB bank
    prg[0x0000] = 0xAA; // first bank start
    prg[0x4000] = 0xBB; // second bank start

    var cart = Cartridge{
        .header = std.mem.zeroes(NesHeader),
        .prgROM = prg[0..],
        .chrROM = null,
    };
    cart.header.prgRomSize = 2; // 32KB

    var mapper = Mapper0{ .cart = &cart };

    // $8000 reads ROM[0x0000] (first bank)
    try std.testing.expectEqual(@as(u8, 0xAA), mapper.read(0x8000));

    // $C000 reads ROM[0x4000] (second bank, NOT mirrored)
    try std.testing.expectEqual(@as(u8, 0xBB), mapper.read(0xC000));

    // These should be different in 32KB mode
    try std.testing.expect(mapper.read(0x8000) != mapper.read(0xC000));
}

test "Mapper0: interface vtable" {
    var prg: [16 * 1024]u8 = undefined;
    @memset(&prg, 0x42);

    var cart = Cartridge{
        .header = std.mem.zeroes(NesHeader),
        .prgROM = prg[0..],
        .chrROM = null,
    };
    cart.header.prgRomSize = 1;

    var mapper = Mapper0{ .cart = &cart };
    var interface = Mapper.init(&mapper);

    // Verify interface calls through to implementation
    try std.testing.expectEqual(@as(u8, 0x42), interface.read(0x8000));

    interface.write(0x6000, 0x99);
    try std.testing.expectEqual(@as(u8, 0x99), interface.read(0x6000));
}

test "Mapper0: factory allocation" {
    const alloc = std.testing.allocator;

    var prg: [16 * 1024]u8 = undefined;
    @memset(&prg, 0xEA); // NOP opcode

    var cart = Cartridge{
        .header = std.mem.zeroes(NesHeader),
        .prgROM = prg[0..],
        .chrROM = null,
    };
    cart.header.prgRomSize = 1;

    var mapper = try mapperFactory(alloc, &cart);
    defer mapper.deinit(alloc);

    try std.testing.expectEqual(@as(u8, 0xEA), mapper.read(0x8000));
    try std.testing.expectEqual(@as(u8, 0xEA), mapper.read(0xFFFC)); // reset vector location
}

test "Mapper0: CHR ROM read" {
    var chr: [8 * 1024]u8 = undefined;
    @memset(&chr, 0x00);
    chr[0x0000] = 0x11;
    chr[0x1000] = 0x22; // pattern table 1
    chr[0x1FFF] = 0x33;

    var cart = Cartridge{
        .header = std.mem.zeroes(NesHeader),
        .prgROM = null,
        .chrROM = chr[0..],
    };

    var mapper = Mapper0{ .cart = &cart };

    try std.testing.expectEqual(@as(u8, 0x11), mapper.ppuRead(0x0000));
    try std.testing.expectEqual(@as(u8, 0x22), mapper.ppuRead(0x1000));
    try std.testing.expectEqual(@as(u8, 0x33), mapper.ppuRead(0x1FFF));

    // Writes to CHR ROM should be ignored
    mapper.ppuWrite(0x0000, 0xFF);
    try std.testing.expectEqual(@as(u8, 0x11), mapper.ppuRead(0x0000));
}

test "Mapper0: CHR RAM read/write" {
    var cart = Cartridge{
        .header = std.mem.zeroes(NesHeader),
        .prgROM = null,
        .chrROM = null, // no CHR ROM = use CHR RAM
    };

    var mapper = Mapper0{ .cart = &cart };

    // Initially zero
    try std.testing.expectEqual(@as(u8, 0x00), mapper.ppuRead(0x0000));

    // Write and read back
    mapper.ppuWrite(0x0000, 0xAA);
    mapper.ppuWrite(0x1000, 0xBB);
    mapper.ppuWrite(0x1FFF, 0xCC);

    try std.testing.expectEqual(@as(u8, 0xAA), mapper.ppuRead(0x0000));
    try std.testing.expectEqual(@as(u8, 0xBB), mapper.ppuRead(0x1000));
    try std.testing.expectEqual(@as(u8, 0xCC), mapper.ppuRead(0x1FFF));
}
