const std = @import("std");
const Cartridge = @import("cartridge.zig").Cartridge;
const NesHeader = @import("cartridge.zig").NesHeader;

// Mapper Interface
const Mapper = struct {
    impl: *anyopaque,
    vtable: *const VTable,

    const VTable = struct {
        read: *const fn (self: *anyopaque, address: u16) u8,
        write: *const fn (self: *anyopaque, address: u16, value: u8) void,
        // ppuRead: *const fn(self: *Mapper, address: u16) u8,
        // ppuWrite: *const fn(self: *Mapper, address: u16, value: u8) void,
        // scanline: *const fn(self: *Mapper, ) void,
        deinit: *const fn (self: *anyopaque, allocator: std.mem.Allocator) void,
    };

    pub fn init(impl: anytype) Mapper {
        const T = std.meta.Child(@TypeOf(impl));
        return .{ .impl = impl, .vtable = &.{
            .read = @ptrCast(&@field(T, "read")),
            .write = @ptrCast(&@field(T, "write")),
            .deinit = @ptrCast(&@field(T, "deinit")),
        } };
    }

    pub fn read(self: *Mapper, address: u16) u8 {
        return self.vtable.read(self.impl, address);
    }

    pub fn write(self: *Mapper, address: u16, value: u8) void {
        return self.vtable.write(self.impl, address, value);
    }

    pub fn deinit(self: *Mapper, allocator: std.mem.Allocator) void {
        return self.vtable.deinit(self.impl, allocator);
    }
};

const Mapper0 = struct {
    cart: *Cartridge,
    prgRam: [0x2000]u8 = std.mem.zeroes([0x2000]u8), // always provide all ram

    // pub fn init(cart: *Cartridge) Mapper0 {
    //     return .{ .cart = cart };
    // }

    // pub fn init(self: *Mapper0, allocator: std.mem.Allocator, cart: *Cartridge) !void {
    //     _ = allocator;
    //     self.cart = cart;
    // }

    fn read(self: *Mapper0, address: u16) u8 {
        // _ = self;
        // _ = address;
        std.debug.print("Mapper0 read\n", .{});
        std.debug.print("self ptr: {*}\n", .{self});
        std.debug.print("self.cart ptr: {*}\n", .{self.cart});

        if (address >= 0x6000 and address < 0x8000) {
            // PRG-RAM
            // TODO handle this better with ines 2.0 header
            return self.prgRam[address];
        } else if (address < 0xC000) {
            //PRG-ROM
            const mirror_addr = if (self.cart.header.prgRamSize == 1) address & 0x3FFF else address;
            return self.prgRam[mirror_addr];
        }

        std.debug.panic("Mapper0: Should not have tried to access ram outside of range addr: 0x{X:0>4}", .{address});

        return 0;
    }

    fn write(self: *Mapper0, address: u16, value: u8) void {
        _ = self;
        _ = address;
        _ = value;
        std.debug.print("Mapper0 write\n", .{});
    }

    fn deinit(self: *Mapper0, allocator: std.mem.Allocator) void {
        allocator.destroy(self);
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

test "Mapper 0" {
    const alloc = std.testing.allocator;

    var cart = Cartridge{ .header = std.mem.zeroes(NesHeader), .prgROM = null, .chrROM = null };

    std.debug.print("cart ptr: {*}\n", .{&cart});

    var mapper = Mapper0{ .cart = &cart };
    _ = mapper.read(0);

    // test interface
    var mapper_interface = Mapper.init(&mapper);
    _ = mapper_interface.read(0);

    // factory test
    var fac_interface = try mapperFactory(alloc, &cart);
    defer fac_interface.deinit(alloc);
    _ = fac_interface.read(0);
}
