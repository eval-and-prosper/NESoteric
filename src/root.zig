//! By convention, root.zig is the root source file when making a library.
const std = @import("std");

// Export emulator features
pub const Cpu = @import("cpu.zig").Cpu;
pub const Bus = @import("cpu.zig").Bus;

test {
    _ = @import("cpu.zig");
    _ = @import("bus.zig");
    _ = @import("instruction_tests.zig");
    _ = @import("addressing_tests.zig");

}

pub fn bufferedPrint() !void {
    // Stdout is for the actual output of your application, for example if you
    // are implementing gzip, then only the compressed bytes should be sent to
    // stdout, not any debugging messages.
    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    try stdout.print("Run `zig build test` to run the tests.\n", .{});

    try stdout.flush(); // Don't forget to flush!
}

pub fn add(a: i32, b: i32) i32 {
    return a + b;
}

test "basic add functionality" {
    try std.testing.expect(add(3, 7) == 10);
}
