const std = @import("std");
const state = @import("state.zig");
const cartHeader = @import("cartHeader.zig");
const cpu = @import("cpu.zig");

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    const rom_file = try std.fs.cwd().openFile(args[1], .{});
    const rom_data: []u8 = try rom_file.readToEndAlloc(allocator, 8192 * 1024);
    defer allocator.free(rom_data);

    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    const header: cartHeader.CartridgeHeader = cartHeader.CartridgeHeader.init(rom_data);
    try header.pp(stdout);

    var st: state.State = state.State.init();

    // TODO: Map rom in to memory correctly, function of header I think...

    while (true) {
        try st.pp(stdout);
        cpu.execute(&st, rom_data);
        try st.pp(stdout);
        try bw.flush();
    }
}

test "Read ROM" {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();
    const rom_path = "/opt/test_roms/cpu_instrs.gb";
    const allocator = std.heap.page_allocator;
    const rom_file = try std.fs.cwd().openFile(rom_path, .{});
    const rom_data: []u8 = try rom_file.readToEndAlloc(allocator, 8192 * 1024);
    defer allocator.free(rom_data);
    try stdout.print("Read in {s} of size {d}\n", .{ rom_path, rom_data.len });
    try bw.flush();
}
