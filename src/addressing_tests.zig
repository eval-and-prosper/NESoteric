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

test "immediate addressing" {
    var ctx = setup();

    ctx.b.write(0, 0x42);
    const result = cpu.resolveAddress(&ctx.c, &ctx.b, .immediate);
    try testing.expectEqual(0x42, ctx.b.read(result.addr));
    try testing.expectEqual(1, ctx.c.programCounter);
}

test "absolute addressing" {
    var ctx = setup();

    ctx.b.write(0, 0x34); // Low byte of address
    ctx.b.write(1, 0x07); // High byte of address
    ctx.b.write(0x734, 0x42); // Value at target address
    ctx.b.write(0x34, 0xFF); // Wrong if high byte ignored
    ctx.b.write(0x735, 0xFF); // Wrong if off by one

    const result = cpu.resolveAddress(&ctx.c, &ctx.b, .absolute);
    try testing.expectEqual(0x42, ctx.b.read(result.addr));
    try testing.expectEqual(2, ctx.c.programCounter);
}

test "zero page addressing" {
    var ctx = setup();

    ctx.b.write(0, 0x42); // Zero page address
    ctx.b.write(0x42, 0x37); // Value at zero page address

    const result = cpu.resolveAddress(&ctx.c, &ctx.b, .zero_page);
    try testing.expectEqual(0x37, ctx.b.read(result.addr));
    try testing.expectEqual(1, ctx.c.programCounter);
}

test "absolute X addressing - normal case" {
    var ctx = setup();

    ctx.c.xRegister = 0x02;
    ctx.b.write(0, 0x34); // Low byte of base address
    ctx.b.write(1, 0x07); // High byte of base address
    ctx.b.write(0x736, 0x42); // Value at target (0x0734 + X)
    ctx.b.write(0x36, 0xFF); // Wrong if high byte ignored

    const result = cpu.resolveAddress(&ctx.c, &ctx.b, .absolute_x);
    try testing.expectEqual(0x42, ctx.b.read(result.addr));
    try testing.expectEqual(2, ctx.c.programCounter);
}

test "absolute X addressing - page boundary crossing" {
    var ctx = setup();

    ctx.c.xRegister = 0xFF;
    ctx.b.write(0, 0x34); // Low byte of base address
    ctx.b.write(1, 0x06); // High byte of base address
    ctx.b.write(0x733, 0x42); // Value at target (0x0634 + 0xFF)
    ctx.b.write(0x33, 0xFF); // Wrong if high byte ignored

    const result = cpu.resolveAddress(&ctx.c, &ctx.b, .absolute_x);
    try testing.expectEqual(0x42, ctx.b.read(result.addr));
    try testing.expectEqual(2, ctx.c.programCounter);
}

test "absolute Y addressing - normal case" {
    var ctx = setup();

    ctx.c.yRegister = 0x02;
    ctx.b.write(0, 0x34); // Low byte of base address
    ctx.b.write(1, 0x07); // High byte of base address
    ctx.b.write(0x736, 0x42); // Value at target (0x0734 + Y)
    ctx.b.write(0x36, 0xFF); // Wrong if high byte ignored

    const result = cpu.resolveAddress(&ctx.c, &ctx.b, .absolute_y);
    try testing.expectEqual(0x42, ctx.b.read(result.addr));
    try testing.expectEqual(2, ctx.c.programCounter);
}

test "absolute Y addressing - page boundary crossing" {
    var ctx = setup();

    ctx.c.yRegister = 0xFF;
    ctx.b.write(0, 0x34); // Low byte of base address
    ctx.b.write(1, 0x06); // High byte of base address
    ctx.b.write(0x733, 0x42); // Value at target (0x0634 + 0xFF)
    ctx.b.write(0x33, 0xFF); // Wrong if high byte ignored

    const result = cpu.resolveAddress(&ctx.c, &ctx.b, .absolute_y);
    try testing.expectEqual(0x42, ctx.b.read(result.addr));
    try testing.expectEqual(2, ctx.c.programCounter);
}

test "zero page X addressing - no wrap" {
    var ctx = setup();

    ctx.c.xRegister = 0x02;
    ctx.b.write(0, 0x42); // Zero page address
    ctx.b.write(0x44, 0x37); // Value at (zero page + X)

    const result = cpu.resolveAddress(&ctx.c, &ctx.b, .zero_page_x);
    try testing.expectEqual(0x37, ctx.b.read(result.addr));
    try testing.expectEqual(1, ctx.c.programCounter);
}

test "zero page X addressing - with wrap" {
    var ctx = setup();

    ctx.c.xRegister = 0x04;
    ctx.b.write(0, 0xFE); // Zero page address
    ctx.b.write(0x02, 0x37); // Value at wrapped address (0xFE + 0x04 = 0x102 -> 0x02)
    ctx.b.write(0x102, 0xFF); // Wrong value if no wrapping occurs

    const result = cpu.resolveAddress(&ctx.c, &ctx.b, .zero_page_x);
    try testing.expectEqual(0x37, ctx.b.read(result.addr));
    try testing.expectEqual(1, ctx.c.programCounter);
}

test "zero page Y addressing - no wrap" {
    var ctx = setup();

    ctx.c.yRegister = 0x02;
    ctx.b.write(0, 0x42); // Zero page address
    ctx.b.write(0x44, 0x37); // Value at (zero page + Y)

    const result = cpu.resolveAddress(&ctx.c, &ctx.b, .zero_page_y);
    try testing.expectEqual(0x37, ctx.b.read(result.addr));
    try testing.expectEqual(1, ctx.c.programCounter);
}

test "zero page Y addressing - with wrap" {
    var ctx = setup();

    ctx.c.yRegister = 0x04;
    ctx.b.write(0, 0xFE); // Zero page address
    ctx.b.write(0x02, 0x37); // Value at wrapped address (0xFE + 0x04 = 0x102 -> 0x02)
    ctx.b.write(0x102, 0xFF); // Wrong value if no wrapping occurs

    const result = cpu.resolveAddress(&ctx.c, &ctx.b, .zero_page_y);
    try testing.expectEqual(0x37, ctx.b.read(result.addr));
    try testing.expectEqual(1, ctx.c.programCounter);
}

test "indirect addressing" {
    var ctx = setup();

    // Write pointer address
    ctx.b.write(0, 0x34); // Low byte of pointer
    ctx.b.write(1, 0x07); // High byte of pointer
    // Write target address at pointer location
    ctx.b.write(0x734, 0x78); // Low byte of target
    ctx.b.write(0x735, 0x06); // High byte of target

    const result = cpu.resolveAddress(&ctx.c, &ctx.b, .indirect);
    try testing.expectEqual(0x0678, result.addr);
    try testing.expectEqual(2, ctx.c.programCounter);
}

test "indirect addressing - page boundary bug" {
    var ctx = setup();

    // Test the 6502 page boundary bug where JMP ($xxFF) reads high byte from $xx00
    ctx.b.write(0, 0xFF); // Low byte of pointer
    ctx.b.write(1, 0x02); // High byte of pointer
    ctx.b.write(0x02FF, 0x34); // Low byte of target at end of page
    ctx.b.write(0x0200, 0x12); // High byte wraps to start of same page (bug behavior)
    ctx.b.write(0x0300, 0xFF); // Wrong if no bug emulation

    const result = cpu.resolveAddress(&ctx.c, &ctx.b, .indirect_bug);
    try testing.expectEqual(0x1234, result.addr);
    try testing.expectEqual(2, ctx.c.programCounter);
}

test "pre-indexed indirect (indirect X) addressing" {
    var ctx = setup();

    ctx.c.xRegister = 0x02;
    // Write zero page address
    ctx.b.write(0, 0x42); // Zero page address
    // Write target address at (zero page + X)
    ctx.b.write(0x44, 0x34); // Low byte of target at (ZP + X)
    ctx.b.write(0x45, 0x07); // High byte of target at (ZP + X)
    // Write test values
    ctx.b.write(0x734, 0x42); // Expected address
    ctx.b.write(0x34, 0xFF); // Wrong if high byte ignored
    ctx.b.write(0x735, 0xFF); // Wrong if off by one

    const result = cpu.resolveAddress(&ctx.c, &ctx.b, .indirect_x);
    try testing.expectEqual(0x42, ctx.b.read(result.addr));
    try testing.expectEqual(1, ctx.c.programCounter);
}

test "pre-indexed indirect (indirect X) - zero page wrapping" {
    var ctx = setup();

    ctx.c.xRegister = 0xFF;
    // Write zero page address
    ctx.b.write(0, 0x42); // Zero page address
    // When 0x42 + 0xFF = 0x141, should wrap to 0x41 in zero page
    // Write target address at wrapped location
    ctx.b.write(0x41, 0x34); // Low byte of target at wrapped (ZP + X)
    ctx.b.write(0x42, 0x07); // High byte of target at wrapped (ZP + X)
    // Write test values
    ctx.b.write(0x734, 0x42); // Expected address
    ctx.b.write(0x141, 0xFF); // Wrong if no wrapping occurs
    ctx.b.write(0x142, 0xFF); // Wrong if no wrapping occurs

    const result = cpu.resolveAddress(&ctx.c, &ctx.b, .indirect_x);
    try testing.expectEqual(0x42, ctx.b.read(result.addr));
    try testing.expectEqual(1, ctx.c.programCounter);
}

test "pre-indexed indirect (indirect X) - zero page boundary read" {
    var ctx = setup();

    ctx.c.xRegister = 0x02;
    ctx.c.programCounter = 0x100;
    // Write zero page address to fetch from
    ctx.b.write(0x100, 0xFD); // Zero page address + X will be 0xFF
    // Write target address crossing zero page boundary
    ctx.b.write(0xFF, 0x34); // Low byte at end of zero page
    ctx.b.write(0x00, 0x07); // High byte wraps to start of zero page
    // Write test value
    ctx.b.write(0x734, 0x42); // Expected address

    const result = cpu.resolveAddress(&ctx.c, &ctx.b, .indirect_x);
    try testing.expectEqual(0x42, ctx.b.read(result.addr));
    try testing.expectEqual(0x101, ctx.c.programCounter);
}

test "post-indexed indirect (indirect Y) addressing" {
    var ctx = setup();

    ctx.c.yRegister = 0x02;
    // Write zero page address
    ctx.b.write(0, 0x42); // Zero page address
    // Write base address at zero page
    ctx.b.write(0x42, 0x34); // Low byte of base address
    ctx.b.write(0x43, 0x07); // High byte of base address
    // Write test values
    ctx.b.write(0x736, 0x42); // Expected address (0x0734 + 2)
    ctx.b.write(0x36, 0xFF); // Wrong if high byte ignored
    ctx.b.write(0x735, 0xFF); // Wrong if off by one

    const result = cpu.resolveAddress(&ctx.c, &ctx.b, .indirect_y);
    try testing.expectEqual(0x42, ctx.b.read(result.addr));
    try testing.expectEqual(1, ctx.c.programCounter);
}

test "post-indexed indirect (indirect Y) - page boundary crossing" {
    var ctx = setup();

    ctx.c.yRegister = 0x02;
    // Write zero page address
    ctx.b.write(0, 0x42); // Zero page address
    // Write base address at zero page that will cross page when Y added
    ctx.b.write(0x42, 0xFF); // Low byte of base address
    ctx.b.write(0x43, 0x07); // High byte of base address
    // Write test value at page-crossed address
    ctx.b.write(0x801, 0x42); // Expected address (0x07FF + 2)

    const result = cpu.resolveAddress(&ctx.c, &ctx.b, .indirect_y);
    try testing.expectEqual(0x42, ctx.b.read(result.addr));
    try testing.expectEqual(1, ctx.c.programCounter);
}

test "post-indexed indirect (indirect Y) - wrapping after Y add" {
    var ctx = setup();

    ctx.c.yRegister = 0x34;
    ctx.b.write(0, 0x97); // Zero page address
    ctx.b.write(0x97, 0xFF); // Low byte of base (0xFFFF will wrap with Y)
    ctx.b.write(0x98, 0xFF); // High byte of base
    ctx.b.write(0x33, 0x12); // Value at wrapped address (0xFFFF + 0x34 = 0x10033 -> 0x0033)

    const result = cpu.resolveAddress(&ctx.c, &ctx.b, .indirect_y);
    try testing.expectEqual(0x12, ctx.b.read(result.addr));
    try testing.expectEqual(1, ctx.c.programCounter);
}

test "post-indexed indirect (indirect Y) - zero page boundary read" {
    var ctx = setup();

    ctx.c.yRegister = 0x02;
    ctx.c.programCounter = 0x100;
    // Write zero page address that will cause boundary read
    ctx.b.write(0x100, 0xFF); // Zero page address at boundary
    // Write base address crossing zero page boundary
    ctx.b.write(0xFF, 0x34); // Low byte at end of zero page
    ctx.b.write(0x00, 0x07); // High byte wraps to start of zero page
    // Write test value
    ctx.b.write(0x736, 0x42); // Expected address (0x0734 + 2)

    const result = cpu.resolveAddress(&ctx.c, &ctx.b, .indirect_y);
    try testing.expectEqual(0x42, ctx.b.read(result.addr));
    try testing.expectEqual(0x101, ctx.c.programCounter);
}

test "relative addressing - maximum positive offset" {
    var ctx = setup();

    ctx.b.write(0, 0x7F); // +127 (maximum positive)
    const offset = cpu.resolveRelativeOffset(&ctx.c, &ctx.b);
    try testing.expectEqual(127, offset);
    try testing.expectEqual(1, ctx.c.programCounter);
}

test "relative addressing - maximum negative offset" {
    var ctx = setup();

    ctx.b.write(0, 0x80); // -128 (maximum negative)
    const offset = cpu.resolveRelativeOffset(&ctx.c, &ctx.b);
    try testing.expectEqual(-128, offset);
    try testing.expectEqual(1, ctx.c.programCounter);
}

test "relative addressing - zero offset" {
    var ctx = setup();

    ctx.b.write(0, 0x00);
    const offset = cpu.resolveRelativeOffset(&ctx.c, &ctx.b);
    try testing.expectEqual(0, offset);
    try testing.expectEqual(1, ctx.c.programCounter);
}

test "relative addressing - small positive offset" {
    var ctx = setup();

    ctx.b.write(0, 0x01); // +1
    const offset = cpu.resolveRelativeOffset(&ctx.c, &ctx.b);
    try testing.expectEqual(1, offset);
    try testing.expectEqual(1, ctx.c.programCounter);
}

test "relative addressing - small negative offset" {
    var ctx = setup();

    ctx.b.write(0, 0xFF); // -1 in two's complement
    const offset = cpu.resolveRelativeOffset(&ctx.c, &ctx.b);
    try testing.expectEqual(-1, offset);
    try testing.expectEqual(1, ctx.c.programCounter);
}