const std = @import("std");

pub const NesHeader = packed struct {
    magic: u32,
    prgRomSize: u8,
    chrRomSize: u8,
    // flags6
    mirroring: bool, // false = horizontal, true = vertical
    battery: bool,
    trainer: bool,
    fourScreen: bool,
    mapperLo: u4,
    // flags7
    vsUnisystem: bool,
    playchoice10: bool,
    nes2Format: u2, // if == 2, this is NES 2.0 format
    mapperHi: u4, // upper nibble of mapper number
    // flags8
    prgRamSize: u8, // in 8KB units, 0 means 8KB for compatibility
    // flags9
    tvSystem: u8, // 0 = NTSC, 1 = PAL (rarely used)
    // flags10
    flags10: u8, // unofficial, rarely used
    unused: u40, // should be zero-filled

    const VALID_MAGIC = std.mem.bytesToValue(u32, "NES\x1a");

    pub fn isValid(self: NesHeader) bool {
        return self.magic == VALID_MAGIC;
    }

    pub fn mapper(self: NesHeader) u8 {
        return @as(u8, self.mapperHi) << 4 | self.mapperLo;
    }

    pub fn print(self: *const NesHeader) void {
        std.debug.print(
            \\Magic: {s}
            \\PRG ROM: {d} x 16KB
            \\CHR ROM: {d} x 8KB
            \\Mapper: {d}
            \\Mirroring: {s}
            \\Battery: {}
            \\Trainer: {}
            \\Four-screen: {}
            \\NES 2.0: {}
            \\PRG RAM: {d} x 8KB
            \\TV System: {s}
            \\
        , .{
            std.mem.asBytes(&self.magic),
            self.prgRomSize,
            self.chrRomSize,
            self.mapper(),
            if (self.mirroring) "Vertical" else "Horizontal",
            self.battery,
            self.trainer,
            self.fourScreen,
            self.nes2Format == 2,
            if (self.prgRamSize == 0) @as(u8, 1) else self.prgRamSize,
            if (self.tvSystem == 0) "NTSC" else "PAL",
        });
    }
};

// should probably parse the header into this
const RomInfo = struct {};

// (defstruct flags6
// (nametable-arrangement)
// (battery-backed-prg-ram)
// (trainer)
// (alt-nametable-layout)
// (low-mapper-number))
//
// (defstruct flags7
// (vs-unisystem)
// (playchoice-10)
// (ines-2)
// (high-mapper-number))
//
// (defstruct flags8
// (prg-ram-size))                       ;unused?
//
// (defstruct flags9                       ;unused?
// (tv-system)
// (reserved))
//
// (defstruct flags10
// (tv-system-2)
// (prg-ram-present)
// (bus-conflict))

pub const Cartridge = struct {
    // data: []u8,
    header: NesHeader,
    prgROM: ?[]u8,
    chrROM: ?[]u8,

    pub fn parse(allocator: std.mem.Allocator, reader: *std.Io.Reader) !Cartridge {
        // var cart = Cartridge{{}, null, null};

        const header_buff = try reader.readAlloc(allocator, @sizeOf(NesHeader));
        defer allocator.free(header_buff);
        const header = std.mem.bytesToValue(NesHeader, header_buff);

        // alternative allocating the header into pointer
        // const hed = try alloc.create(NesHeader);
        // defer alloc.destroy(hed);
        // _ = try reader.readSliceAll(std.mem.asBytes(hed));

        if(header.trainer) {
            // @panic("Cartridge: We don't handle trainers yet.");
            _ = try reader.discardShort(512);
        }

        const kb: usize = 1024;
        const prg_size = 16 * kb * header.prgRomSize;
        var prg: ?[]u8 = null;
        if (prg_size > 0) {
            prg = try reader.readAlloc(allocator, prg_size);
        }
        errdefer if (prg) |p| allocator.free(p);

        const chr_size = 8 * kb * header.chrRomSize;
        var chr: ?[]u8 = null;
        if (chr_size > 0) {
            chr = try reader.readAlloc(allocator, chr_size);
        }

        // return .{ header, prg, chr };
        return .{ .header = header, .prgROM = prg, .chrROM = chr };

    }

    pub fn deinit(cart: *Cartridge, allocator: std.mem.Allocator) void {
        if (cart.prgROM) |prg| {
            allocator.free(prg);
        }
        if (cart.chrROM) |chr| {
            allocator.free(chr);
        }
    }
};

test "parse cartridge from memory" {
    const alloc = std.testing.allocator;

    // Build a minimal valid iNES ROM in memory:
    // - 16 byte header
    // - 1 x 16KB PRG ROM
    // - 1 x 8KB CHR ROM
    const prg_size = 16 * 1024;
    const chr_size = 8 * 1024;
    const total_size = @sizeOf(NesHeader) + prg_size + chr_size;

    var rom_data: [total_size]u8 = undefined;

    // Write header
    const header = NesHeader{
        .magic = NesHeader.VALID_MAGIC,
        .prgRomSize = 1,
        .chrRomSize = 1,
        .mirroring = true, // vertical
        .battery = false,
        .trainer = false,
        .fourScreen = false,
        .mapperLo = 0,
        .vsUnisystem = false,
        .playchoice10 = false,
        .nes2Format = 0,
        .mapperHi = 0,
        .prgRamSize = 0,
        .tvSystem = 0,
        .flags10 = 0,
        .unused = 0,
    };
    @memcpy(rom_data[0..@sizeOf(NesHeader)], std.mem.asBytes(&header));

    // Fill PRG ROM with pattern (0x00, 0x01, 0x02, ...)
    for (0..prg_size) |i| {
        rom_data[@sizeOf(NesHeader) + i] = @truncate(i);
    }

    // Fill CHR ROM with different pattern (0xFF, 0xFE, ...)
    for (0..chr_size) |i| {
        rom_data[@sizeOf(NesHeader) + prg_size + i] = @truncate(0xFF -% @as(u8, @truncate(i)));
    }

    // Create reader from fixed buffer
    var reader = std.Io.Reader.fixed(&rom_data);

    var cart = try Cartridge.parse(alloc, &reader);
    defer cart.deinit(alloc);

    // Verify header
    try std.testing.expect(cart.header.isValid());
    try std.testing.expectEqual(@as(u8, 1), cart.header.prgRomSize);
    try std.testing.expectEqual(@as(u8, 1), cart.header.chrRomSize);
    try std.testing.expect(cart.header.mirroring);
    try std.testing.expectEqual(@as(u8, 0), cart.header.mapper());

    // Verify PRG ROM
    try std.testing.expect(cart.prgROM != null);
    try std.testing.expectEqual(prg_size, cart.prgROM.?.len);
    try std.testing.expectEqual(@as(u8, 0x00), cart.prgROM.?[0]);
    try std.testing.expectEqual(@as(u8, 0x01), cart.prgROM.?[1]);

    // Verify CHR ROM
    try std.testing.expect(cart.chrROM != null);
    try std.testing.expectEqual(chr_size, cart.chrROM.?.len);
    try std.testing.expectEqual(@as(u8, 0xFF), cart.chrROM.?[0]);
    try std.testing.expectEqual(@as(u8, 0xFE), cart.chrROM.?[1]);

    // Verify we consumed exactly the entire ROM - reading one more byte should fail
    try std.testing.expectError(error.EndOfStream, reader.takeByte());
}
