const std = @import("std");

var State = struct {
    memory: [0x10000]u8,
    reg_AF: u16,
    reg_BC: u16,
    reg_DE: u16,
    reg_HL: u16,
    reg_SP: u16,
    reg_PC: u16,
};

const Regs = enum {AF, A, F, BC, B, C, DE, D, E, HL, H, L, SP, PC};

const Flags = enum {Z, N, H, C};

pub fn setReg(reg: Regs, val: u16!u8) void {
        
}

pub fn main() !void {
    // Prints to stderr (it's a shortcut based on `std.io.getStdErr()`)
    std.debug.print("All your {s} are belong to us.\n", .{"codebase"});

    // stdout is for the actual output of your application, for example if you
    // are implementing gzip, then only the compressed bytes should be sent to
    // stdout, not any debugging messages.
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    try stdout.print("Run `zig build test` to run the tests.\n", .{});

    try bw.flush(); // don't forget to flush!
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
