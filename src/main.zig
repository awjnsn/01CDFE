const std = @import("std");
const state = @import("state.zig");

pub fn main() !void {
    // Prints to stderr (it's a shortcut based on `std.io.getStdErr()`)
    //std.debug.print("All your {s} are belong to us.\n", .{"codebase"});

    // stdout is for the actual output of your application, for example if you
    // are implementing gzip, then only the compressed bytes should be sent to
    // stdout, not any debugging messages.
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    var testState: state.State = state.State.init();

    try stdout.print("Initial state\n\n", .{});

    try testState.pp(stdout);

    try stdout.print("Changing PC\n\n", .{});

    testState.setReg(state.Regs.PC, 0xBEEF);

    try testState.pp(stdout);

    try stdout.print("Setting Z flag\n\n", .{});

    testState.setFlag(state.Flags.Z, true);

    try testState.pp(stdout);

    try bw.flush(); // don't forget to flush!
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
