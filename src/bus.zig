const std = @import("std");
const testing = std.testing;

const Mapper = @import("mapper.zig").Mapper;

pub const Bus = struct {
    const ram_size = 0x800;

    mapper: ?*Mapper = null,
    ram: [ram_size]u8 = [_]u8{0} ** ram_size,

    pub fn read(self: *Bus, address: u16) u8 {
        return switch (address) {
            0x0000...0x1FFF => self.ram[address & 0x07FF], // RAM (mirrored)
            0x2000...0x3FFF => 0, // TODO PPU registers (mirrored)
            0x4000...0x401F => 0, // TODO APU and I/O registers
            0x4020...0xFFFF => if (self.mapper) |mapper| mapper.read(address) else 0, // Cartridge
        };
    }

    pub fn read16(self: *Bus, address: u16) u16 {
        const lo = self.read(address);
        const hi = self.read(address +% 1);
        return @as(u16, hi) << 8 | lo;
    }

    pub fn read16ZeroPage(self: *Bus, address: u8) u16 {
        const lo = self.read(address);
        const hi = self.read(address +% 1); // Wraps within zero page (u8 arithmetic)
        return @as(u16, hi) << 8 | lo;
    }

    pub fn write(self: *Bus, address: u16, value: u8) void {
        switch (address) {
            0x0000...0x1FFF => self.ram[address & 0x07FF] = value, // RAM (mirrored)
            0x2000...0x3FFF => {}, // TODO PPU registers (mirrored)
            0x4000...0x401F => {}, // TODO APU and I/O registers
            0x4020...0xFFFF => if (self.mapper) |mapper| mapper.write(address, value), // Cartridge
        }
    }

    pub fn write16(self: *Bus, address: u16, value: u16) void {
        self.write(address, @truncate(value));
        self.write(address +% 1, @truncate(value >> 8));
    }
};

test "Bus" {
    var bus = Bus{};

    bus.write(0, 0xAA);
    try testing.expectEqual(0xAA, bus.read(0));

    //last in ram
    bus.write(0x7FF, 0xAB);
    try testing.expectEqual(0xAB, bus.read(0x7FF));

    //mirroring
    bus.write(0x0800, 0xAC);
    try testing.expectEqual(0xAC, bus.read(0x800));
    try testing.expectEqual(0xAC, bus.read(0));

    bus.write16(10, 0xDEAD);
    try testing.expectEqual(0xDEAD, bus.read16(10));

    //little endian order
    try testing.expectEqual(0xAD, bus.read(10));
    try testing.expectEqual(0xDE, bus.read(11));

    // mirroring
    bus.write16(0x7FF, 0xCAFE);
    try testing.expectEqual(0xCAFE, bus.read16(0x7FF));

    // mirrored with little endian order
    try testing.expectEqual(0xFE, bus.read(0x7FF));
    try testing.expectEqual(0xCA, bus.read(0));
}
