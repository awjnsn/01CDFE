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
        //0
        0x00 => instruction.nop(),
        0x01 => unreachable,
        0x02 => unreachable,
        0x03 => unreachable,
        0x04 => unreachable,
        0x05 => unreachable,
        0x06 => unreachable,
        0x07 => unreachable,
        0x08 => unreachable,
        0x09 => unreachable,
        0x0A => unreachable,
        0x0B => unreachable,
        0x0C => unreachable,
        0x0D => unreachable,
        0x0E => unreachable,
        0x0F => unreachable,
        //1
        0x10 => unreachable,
        0x11 => unreachable,
        0x12 => unreachable,
        0x13 => unreachable,
        0x14 => unreachable,
        0x15 => unreachable,
        0x16 => unreachable,
        0x17 => unreachable,
        0x18 => unreachable,
        0x19 => unreachable,
        0x1A => unreachable,
        0x1B => unreachable,
        0x1C => unreachable,
        0x1D => unreachable,
        0x1E => unreachable,
        0x1F => unreachable,
        //2
        0x20 => unreachable,
        0x21 => unreachable,
        0x22 => unreachable,
        0x23 => unreachable,
        0x24 => unreachable,
        0x25 => unreachable,
        0x26 => unreachable,
        0x27 => unreachable,
        0x28 => unreachable,
        0x29 => unreachable,
        0x2A => unreachable,
        0x2B => unreachable,
        0x2C => unreachable,
        0x2D => unreachable,
        0x2E => unreachable,
        0x2F => unreachable,
        //3
        0x30 => unreachable,
        0x31 => unreachable,
        0x32 => unreachable,
        0x33 => unreachable,
        0x34 => unreachable,
        0x35 => unreachable,
        0x36 => unreachable,
        0x37 => unreachable,
        0x38 => unreachable,
        0x39 => unreachable,
        0x3A => unreachable,
        0x3B => unreachable,
        0x3C => unreachable,
        0x3D => unreachable,
        0x3E => unreachable,
        0x3F => unreachable,
        //4
        0x40 => unreachable,
        0x41 => unreachable,
        0x42 => unreachable,
        0x43 => unreachable,
        0x44 => unreachable,
        0x45 => unreachable,
        0x46 => unreachable,
        0x47 => unreachable,
        0x48 => unreachable,
        0x49 => unreachable,
        0x4A => unreachable,
        0x4B => unreachable,
        0x4C => unreachable,
        0x4D => unreachable,
        0x4E => unreachable,
        0x4F => unreachable,
        //5
        0x50 => unreachable,
        0x51 => unreachable,
        0x52 => unreachable,
        0x53 => unreachable,
        0x54 => unreachable,
        0x55 => unreachable,
        0x56 => unreachable,
        0x57 => unreachable,
        0x58 => unreachable,
        0x59 => unreachable,
        0x5A => unreachable,
        0x5B => unreachable,
        0x5C => unreachable,
        0x5D => unreachable,
        0x5E => unreachable,
        0x5F => unreachable,
        //6
        0x60 => unreachable,
        0x61 => unreachable,
        0x62 => unreachable,
        0x63 => unreachable,
        0x64 => unreachable,
        0x65 => unreachable,
        0x66 => unreachable,
        0x67 => unreachable,
        0x68 => unreachable,
        0x69 => unreachable,
        0x6A => unreachable,
        0x6B => unreachable,
        0x6C => unreachable,
        0x6D => unreachable,
        0x6E => unreachable,
        0x6F => unreachable,
        //7
        0x70 => unreachable,
        0x71 => unreachable,
        0x72 => unreachable,
        0x73 => unreachable,
        0x74 => unreachable,
        0x75 => unreachable,
        0x76 => unreachable,
        0x77 => unreachable,
        0x78 => unreachable,
        0x79 => unreachable,
        0x7A => unreachable,
        0x7B => unreachable,
        0x7C => unreachable,
        0x7D => unreachable,
        0x7E => unreachable,
        0x7F => unreachable,
        //8
        0x80 => unreachable,
        0x81 => unreachable,
        0x82 => unreachable,
        0x83 => unreachable,
        0x84 => unreachable,
        0x85 => unreachable,
        0x86 => unreachable,
        0x87 => unreachable,
        0x88 => unreachable,
        0x89 => unreachable,
        0x8A => unreachable,
        0x8B => unreachable,
        0x8C => unreachable,
        0x8D => unreachable,
        0x8E => unreachable,
        0x8F => unreachable,
        //9
        0x90 => unreachable,
        0x91 => unreachable,
        0x92 => unreachable,
        0x93 => unreachable,
        0x94 => unreachable,
        0x95 => unreachable,
        0x96 => unreachable,
        0x97 => unreachable,
        0x98 => unreachable,
        0x99 => unreachable,
        0x9A => unreachable,
        0x9B => unreachable,
        0x9C => unreachable,
        0x9D => unreachable,
        0x9E => unreachable,
        0x9F => unreachable,
        //A
        0xA0 => unreachable,
        0xA1 => unreachable,
        0xA2 => unreachable,
        0xA3 => unreachable,
        0xA4 => unreachable,
        0xA5 => unreachable,
        0xA6 => unreachable,
        0xA7 => unreachable,
        0xA8 => unreachable,
        0xA9 => unreachable,
        0xAA => unreachable,
        0xAB => unreachable,
        0xAC => unreachable,
        0xAD => unreachable,
        0xAE => unreachable,
        0xAF => unreachable,
        //B
        0xB0 => unreachable,
        0xB1 => unreachable,
        0xB2 => unreachable,
        0xB3 => unreachable,
        0xB4 => unreachable,
        0xB5 => unreachable,
        0xB6 => unreachable,
        0xB7 => unreachable,
        0xB8 => unreachable,
        0xB9 => unreachable,
        0xBA => unreachable,
        0xBB => unreachable,
        0xBC => unreachable,
        0xBD => unreachable,
        0xBE => unreachable,
        0xBF => unreachable,
        //C
        0xC0 => unreachable,
        0xC1 => unreachable,
        0xC2 => unreachable,
        0xC3 => unreachable,
        0xC4 => unreachable,
        0xC5 => unreachable,
        0xC6 => unreachable,
        0xC7 => unreachable,
        0xC8 => unreachable,
        0xC9 => unreachable,
        0xCA => unreachable,
        0xCB => switch (cb_insn) {
            //0
            0x00 => unreachable,
            0x01 => unreachable,
            0x02 => unreachable,
            0x03 => unreachable,
            0x04 => unreachable,
            0x05 => unreachable,
            0x06 => unreachable,
            0x07 => unreachable,
            0x08 => unreachable,
            0x09 => unreachable,
            0x0A => unreachable,
            0x0B => unreachable,
            0x0C => unreachable,
            0x0D => unreachable,
            0x0E => unreachable,
            0x0F => unreachable,
            //1
            0x10 => unreachable,
            0x11 => unreachable,
            0x12 => unreachable,
            0x13 => unreachable,
            0x14 => unreachable,
            0x15 => unreachable,
            0x16 => unreachable,
            0x17 => unreachable,
            0x18 => unreachable,
            0x19 => unreachable,
            0x1A => unreachable,
            0x1B => unreachable,
            0x1C => unreachable,
            0x1D => unreachable,
            0x1E => unreachable,
            0x1F => unreachable,
            //2
            0x20 => unreachable,
            0x21 => unreachable,
            0x22 => unreachable,
            0x23 => unreachable,
            0x24 => unreachable,
            0x25 => unreachable,
            0x26 => unreachable,
            0x27 => unreachable,
            0x28 => unreachable,
            0x29 => unreachable,
            0x2A => unreachable,
            0x2B => unreachable,
            0x2C => unreachable,
            0x2D => unreachable,
            0x2E => unreachable,
            0x2F => unreachable,
            //3
            0x30 => unreachable,
            0x31 => unreachable,
            0x32 => unreachable,
            0x33 => unreachable,
            0x34 => unreachable,
            0x35 => unreachable,
            0x36 => unreachable,
            0x37 => unreachable,
            0x38 => unreachable,
            0x39 => unreachable,
            0x3A => unreachable,
            0x3B => unreachable,
            0x3C => unreachable,
            0x3D => unreachable,
            0x3E => unreachable,
            0x3F => unreachable,
            //4
            0x40 => unreachable,
            0x41 => unreachable,
            0x42 => unreachable,
            0x43 => unreachable,
            0x44 => unreachable,
            0x45 => unreachable,
            0x46 => unreachable,
            0x47 => unreachable,
            0x48 => unreachable,
            0x49 => unreachable,
            0x4A => unreachable,
            0x4B => unreachable,
            0x4C => unreachable,
            0x4D => unreachable,
            0x4E => unreachable,
            0x4F => unreachable,
            //5
            0x50 => unreachable,
            0x51 => unreachable,
            0x52 => unreachable,
            0x53 => unreachable,
            0x54 => unreachable,
            0x55 => unreachable,
            0x56 => unreachable,
            0x57 => unreachable,
            0x58 => unreachable,
            0x59 => unreachable,
            0x5A => unreachable,
            0x5B => unreachable,
            0x5C => unreachable,
            0x5D => unreachable,
            0x5E => unreachable,
            0x5F => unreachable,
            //6
            0x60 => unreachable,
            0x61 => unreachable,
            0x62 => unreachable,
            0x63 => unreachable,
            0x64 => unreachable,
            0x65 => unreachable,
            0x66 => unreachable,
            0x67 => unreachable,
            0x68 => unreachable,
            0x69 => unreachable,
            0x6A => unreachable,
            0x6B => unreachable,
            0x6C => unreachable,
            0x6D => unreachable,
            0x6E => unreachable,
            0x6F => unreachable,
            //7
            0x70 => unreachable,
            0x71 => unreachable,
            0x72 => unreachable,
            0x73 => unreachable,
            0x74 => unreachable,
            0x75 => unreachable,
            0x76 => unreachable,
            0x77 => unreachable,
            0x78 => unreachable,
            0x79 => unreachable,
            0x7A => unreachable,
            0x7B => unreachable,
            0x7C => unreachable,
            0x7D => unreachable,
            0x7E => unreachable,
            0x7F => unreachable,
            //8
            0x80 => unreachable,
            0x81 => unreachable,
            0x82 => unreachable,
            0x83 => unreachable,
            0x84 => unreachable,
            0x85 => unreachable,
            0x86 => unreachable,
            0x87 => unreachable,
            0x88 => unreachable,
            0x89 => unreachable,
            0x8A => unreachable,
            0x8B => unreachable,
            0x8C => unreachable,
            0x8D => unreachable,
            0x8E => unreachable,
            0x8F => unreachable,
            //9
            0x90 => unreachable,
            0x91 => unreachable,
            0x92 => unreachable,
            0x93 => unreachable,
            0x94 => unreachable,
            0x95 => unreachable,
            0x96 => unreachable,
            0x97 => unreachable,
            0x98 => unreachable,
            0x99 => unreachable,
            0x9A => unreachable,
            0x9B => unreachable,
            0x9C => unreachable,
            0x9D => unreachable,
            0x9E => unreachable,
            0x9F => unreachable,
            //A
            0xA0 => unreachable,
            0xA1 => unreachable,
            0xA2 => unreachable,
            0xA3 => unreachable,
            0xA4 => unreachable,
            0xA5 => unreachable,
            0xA6 => unreachable,
            0xA7 => unreachable,
            0xA8 => unreachable,
            0xA9 => unreachable,
            0xAA => unreachable,
            0xAB => unreachable,
            0xAC => unreachable,
            0xAD => unreachable,
            0xAE => unreachable,
            0xAF => unreachable,
            //B
            0xB0 => unreachable,
            0xB1 => unreachable,
            0xB2 => unreachable,
            0xB3 => unreachable,
            0xB4 => unreachable,
            0xB5 => unreachable,
            0xB6 => unreachable,
            0xB7 => unreachable,
            0xB8 => unreachable,
            0xB9 => unreachable,
            0xBA => unreachable,
            0xBB => unreachable,
            0xBC => unreachable,
            0xBD => unreachable,
            0xBE => unreachable,
            0xBF => unreachable,
            //C
            0xC0 => unreachable,
            0xC1 => unreachable,
            0xC2 => unreachable,
            0xC3 => unreachable,
            0xC4 => unreachable,
            0xC5 => unreachable,
            0xC6 => unreachable,
            0xC7 => unreachable,
            0xC8 => unreachable,
            0xC9 => unreachable,
            0xCA => unreachable,
            0xCB => unreachable,
            0xCC => unreachable,
            0xCD => unreachable,
            0xCE => unreachable,
            0xCF => unreachable,
            //D
            0xD0 => unreachable,
            0xD1 => unreachable,
            0xD2 => unreachable,
            0xD3 => unreachable,
            0xD4 => unreachable,
            0xD5 => unreachable,
            0xD6 => unreachable,
            0xD7 => unreachable,
            0xD8 => unreachable,
            0xD9 => unreachable,
            0xDA => unreachable,
            0xDB => unreachable,
            0xDC => unreachable,
            0xDD => unreachable,
            0xDE => unreachable,
            0xDF => unreachable,
            //E
            0xE0 => unreachable,
            0xE1 => unreachable,
            0xE2 => unreachable,
            0xE3 => unreachable,
            0xE4 => unreachable,
            0xE5 => unreachable,
            0xE6 => unreachable,
            0xE7 => unreachable,
            0xE8 => unreachable,
            0xE9 => unreachable,
            0xEA => unreachable,
            0xEB => unreachable,
            0xEC => unreachable,
            0xED => unreachable,
            0xEE => unreachable,
            0xEF => unreachable,
            //F
            0xF0 => unreachable,
            0xF1 => unreachable,
            0xF2 => unreachable,
            0xF3 => unreachable,
            0xF4 => unreachable,
            0xF5 => unreachable,
            0xF6 => unreachable,
            0xF7 => unreachable,
            0xF8 => unreachable,
            0xF9 => unreachable,
            0xFA => unreachable,
            0xFB => unreachable,
            0xFC => unreachable,
            0xFD => unreachable,
            0xFE => unreachable,
            0xFF => unreachable,
        },
        0xCC => unreachable,
        0xCD => unreachable,
        0xCE => unreachable,
        0xCF => unreachable,
        //D
        0xD0 => unreachable,
        0xD1 => unreachable,
        0xD2 => unreachable,
        0xD3 => unreachable,
        0xD4 => unreachable,
        0xD5 => unreachable,
        0xD6 => unreachable,
        0xD7 => unreachable,
        0xD8 => unreachable,
        0xD9 => unreachable,
        0xDA => unreachable,
        0xDB => unreachable,
        0xDC => unreachable,
        0xDD => unreachable,
        0xDE => unreachable,
        0xDF => unreachable,
        //E
        0xE0 => unreachable,
        0xE1 => unreachable,
        0xE2 => unreachable,
        0xE3 => unreachable,
        0xE4 => unreachable,
        0xE5 => unreachable,
        0xE6 => unreachable,
        0xE7 => unreachable,
        0xE8 => unreachable,
        0xE9 => unreachable,
        0xEA => unreachable,
        0xEB => unreachable,
        0xEC => unreachable,
        0xED => unreachable,
        0xEE => unreachable,
        0xEF => unreachable,
        //F
        0xF0 => unreachable,
        0xF1 => unreachable,
        0xF2 => unreachable,
        0xF3 => unreachable,
        0xF4 => unreachable,
        0xF5 => unreachable,
        0xF6 => unreachable,
        0xF7 => unreachable,
        0xF8 => unreachable,
        0xF9 => unreachable,
        0xFA => unreachable,
        0xFB => unreachable,
        0xFC => unreachable,
        0xFD => unreachable,
        0xFE => unreachable,
        0xFF => unreachable,
    };

    std.debug.print("This instruction took 0x{x} cycles\n", .{cycles});
}
