const std = @import("std");
const state = @import("state.zig");

// Execute the instruction pointed to by the PC register
pub fn execute(st: *state.State, rom_data: []u8) void {
    executeAt(st.getReg(state.Regs.PC), st, rom_data);
}

// Execute the instruction pointed to by address
pub fn executeAt(address: u16, st: *state.State, rom_data: []u8) void {
    std.debug.print(
        "Executing insn at 0x{x}, PC: 0x{x}, first insn byte 0x{x}\n",
        .{address, st.getReg(state.Regs.PC), rom_data[address]}
    );
}
