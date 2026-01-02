const std = @import("std");
const testing = std.testing;

pub const Bus = struct {
    const ram_size = 0x800;
    ram: [ram_size]u8 = [_]u8{0} ** ram_size,
    // TODO temporary - vectors at $FFFA-$FFFF (NMI, Reset, IRQ)
    vectors: [6]u8 = [_]u8{0} ** 6,

    pub fn read(self: *Bus, address: u16) u8 {
        // TODO temporary - handle vectors
        if (address >= 0xFFFA) {
            return self.vectors[address - 0xFFFA];
        } else if (address < 0x2000) {
            //ram
            return self.ram[address & 0x07FF];
        } else if (address < 0x4000) {
            //ppu
        } else if (address < 0x4020) {
            //apu
        } else {
            //cart
        }

        //temporary and wrong
        return 0;
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
        // TODO temporary - handle vectors
        if (address >= 0xFFFA) {
            self.vectors[address - 0xFFFA] = value;
        } else if (address < 0x2000) {
            //ram
            self.ram[address & 0x07FF] = value;
        } else if (address < 0x4000) {
            //ppu
        } else if (address < 0x4020) {
            //apu
        } else {
            //cart
        }
    }

    pub fn write16(self: *Bus, address: u16, value: u16) void {
        self.write(address, @truncate(value));
        self.write(address +% 1, @truncate(value >> 8));
    }
};

test "bus" {
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
