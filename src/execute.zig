const std = @import("std");
const state = @import("state.zig");

// Execute the instruction pointed to by the PC register
pub fn execute(st: *state.State, rom_data: []u8) void {
    //std.debug.print("EXECUTING AT 0x{x}\n {x}", .{st.getReg(state.Regs.PC), rom_data[0]});
    executeAt(st.getReg(state.Regs.PC), st, rom_data);
}

// Execute the instruction pointed to be address
pub fn executeAt(address: u16, st: *state.State, rom_data: []u8) void {
    std.debug.print("Executing insn at 0x{x}, PC: 0x{x}, first insn byte 0x{x}\n",
        .{address, st.getReg(state.Regs.PC), rom_data[address]});
}
//
//test "State Change" {
//    const stdout_file = std.io.getStdOut().writer();
//    var bw = std.io.bufferedWriter(stdout_file);
//    const stdout = bw.writer();
//    var testState: State = State.init();
//    try stdout.print("Initial state\n\n", .{});
//    try testState.pp(stdout);
//    try stdout.print("Changing PC\n\n", .{});
//    testState.setReg(Regs.PC, 0xBEEF);
//    try std.testing.expectEqual(testState.getReg(Regs.PC), 0xBEEF);
//    try testState.pp(stdout);
//    try stdout.print("Setting Z flag\n\n", .{});
//    testState.setFlag(Flags.Z, true);
//    try std.testing.expectEqual(testState.getFlag(Flags.Z), true);
//    try testState.pp(stdout);
//    try bw.flush();
//}
