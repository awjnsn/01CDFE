const std = @import("std");
const st = @import("state.zig");
const insn = @import("instructions.zig");

// Execute the instruction pointed to by the PC register
pub fn execute(state: *st.State, rom_data: []u8) void {
    executeAt(state.getReg(st.Regs.PC), state, rom_data);
}

// Execute the instruction pointed to by address
pub fn executeAt(address: u16, state: *st.State, rom_data: []u8) void {
    std.debug.print(
        "Executing insn at 0x{x}, PC: 0x{x}, first insn byte 0x{x}\n",
        .{address, state.getReg(st.Regs.PC), rom_data[address]}
    );

    const instruction: insn.Instruction = insn.Instruction.init(state);
    const cb_insn: u8 = if (address + 1 < rom_data.len) rom_data[address + 1] else undefined;
    const d8: u8 = if (address + 1 < rom_data.len) rom_data[address + 1] else undefined;
    const d16: u16 = if (address + 2 < rom_data.len) (@as(u16, rom_data[address + 1]) << 8) + rom_data[address + 2] else undefined;
    const a8: u8 = if (address + 1 < rom_data.len) rom_data[address + 1] else undefined;
    const a16: u16 = if (address + 2 < rom_data.len) (@as(u16, rom_data[address + 1]) << 8) + rom_data[address + 2] else undefined;
    const r8: u8 = if (address + 1 < rom_data.len) rom_data[address + 1] else undefined;

    std.debug.print(
        "cb_insn: 0x{x} d8: 0x{x} d16: 0x{x} a8: 0x{x} a16: 0x{x} r8: 0x{x}\n",
        .{cb_insn, d8, d16, a8, a16, r8}
    );

    const cycles: u8 = switch(rom_data[address]) {
        0x0 => instruction.nop(),
        else => unreachable,
    };

    std.debug.print("This instruction took 0x{x} cycles\n", .{cycles});
}
