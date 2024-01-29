const std = @import("std");
const st = @import("state.zig");
const insn = @import("instructions.zig");

// Execute the instruction pointed to by the PC register
pub fn execute(state: *st.State, rom_data: []u8) void {
    executeAt(state.getReg(st.Regs.PC), state, rom_data);
}

// Execute the instruction pointed to by address
pub fn executeAt(address: u16, state: *st.State, rom_data: []u8) void {
    std.debug.print("Executing insn at 0x{x}, PC: 0x{x}, first insn byte 0x{x}\n", .{ address, state.getReg(st.Regs.PC), rom_data[address] });

    const instruction: insn.Instruction = insn.Instruction.init(state);
    const cb_insn: u8 = if (address + 1 < state.memory.len) state.memory[address + 1] else undefined;
    const d8: u8 = if (address + 1 < state.memory.len) state.memory[address + 1] else undefined;
    const d16: u16 = if (address + 2 < state.memory.len) (@as(u16, state.memory[address + 2]) << 8) + state.memory[address + 1] else undefined;
    const a8: u8 = if (address + 1 < state.memory.len) state.memory[address + 1] else undefined;
    const a16: u16 = if (address + 2 < state.memory.len) (@as(u16, state.memory[address + 2]) << 8) + state.memory[address + 1] else undefined;
    const r8: u8 = if (address + 1 < state.memory.len) state.memory[address + 1] else undefined;

    std.debug.print("cb_insn: 0x{x} d8: 0x{x} d16: 0x{x} a8: 0x{x} a16: 0x{x} r8: 0x{x}\n", .{ cb_insn, d8, d16, a8, a16, r8 });

    // See https://www.pastraiser.com/cpu/gameboy/gameboy_opcodes.html
    const cycles: u8 = switch (state.memory[address]) {
        // NOP
        // 1  4
        // - - - -
        0x00 => instruction.nop(),
        // LD BC,d16
        // 3  12
        // - - - -
        0x01 => unreachable,
        // LD (BC),A
        // 1  8
        // - - - -
        0x02 => unreachable,
        // INC BC
        // 1  8
        // - - - -
        0x03 => unreachable,
        // INC B
        // 1  4
        // Z 0 H -
        0x04 => unreachable,
        // DEC B
        // 1  4
        // Z 1 H -
        0x05 => unreachable,
        // LD B,d8
        // 2  8
        // - - - -
        0x06 => unreachable,
        // RLCA
        // 1  4
        // 0 0 0 C
        0x07 => unreachable,
        // LD (a16),SP
        // 3  20
        // - - - -
        0x08 => unreachable,
        // ADD HL,BC
        // 1  8
        // - 0 H C
        0x09 => unreachable,
        // LD A,(BC)
        // 1  8
        // - - - -
        0x0A => unreachable,
        // DEC BC
        // 1  8
        // - - - -
        0x0B => unreachable,
        // INC C
        // 1  4
        // Z 0 H -
        0x0C => unreachable,
        // DEC C
        // 1  4
        // Z 1 H -
        0x0D => unreachable,
        // LD C,d8
        // 2  8
        // - - - -
        0x0E => unreachable,
        // RRCA
        // 1  4
        // 0 0 0 C
        0x0F => unreachable,
        // STOP 0
        // 2  4
        // - - - -
        0x10 => unreachable,
        // LD DE,d16
        // 3  12
        // - - - -
        0x11 => unreachable,
        // LD (DE),A
        // 1  8
        // - - - -
        0x12 => unreachable,
        // INC DE
        // 1  8
        // - - - -
        0x13 => unreachable,
        // INC D
        // 1  4
        // Z 0 H -
        0x14 => unreachable,
        // DEC D
        // 1  4
        // Z 1 H -
        0x15 => unreachable,
        // LD D,d8
        // 2  8
        // - - - -
        0x16 => unreachable,
        // RLA
        // 1  4
        // 0 0 0 C
        0x17 => unreachable,
        // JR r8
        // 2  12
        // - - - -
        0x18 => unreachable,
        // ADD HL,DE
        // 1  8
        // - 0 H C
        0x19 => unreachable,
        // LD A,(DE)
        // 1  8
        // - - - -
        0x1A => unreachable,
        // DEC DE
        // 1  8
        // - - - -
        0x1B => unreachable,
        // INC E
        // 1  4
        // Z 0 H -
        0x1C => unreachable,
        // DEC E
        // 1  4
        // Z 1 H -
        0x1D => unreachable,
        // LD E,d8
        // 2  8
        // - - - -
        0x1E => unreachable,
        // RRA
        // 1  4
        // 0 0 0 C
        0x1F => unreachable,
        // JR NZ,r8
        // 2  12/8
        // - - - -
        0x20 => unreachable,
        // LD HL,d16
        // 3  12
        // - - - -
        0x21 => unreachable,
        // LD (HL+),A
        // 1  8
        // - - - -
        0x22 => unreachable,
        // INC HL
        // 1  8
        // - - - -
        0x23 => unreachable,
        // INC H
        // 1  4
        // Z 0 H -
        0x24 => unreachable,
        // DEC H
        // 1  4
        // Z 1 H -
        0x25 => unreachable,
        // LD H,d8
        // 2  8
        // - - - -
        0x26 => unreachable,
        // DAA
        // 1  4
        // Z - 0 C
        0x27 => unreachable,
        // JR Z,r8
        // 2  12/8
        // - - - -
        0x28 => unreachable,
        // ADD HL,HL
        // 1  8
        // - 0 H C
        0x29 => unreachable,
        // LD A,(HL+)
        // 1  8
        // - - - -
        0x2A => unreachable,
        // DEC HL
        // 1  8
        // - - - -
        0x2B => unreachable,
        // INC L
        // 1  4
        // Z 0 H -
        0x2C => unreachable,
        // DEC L
        // 1  4
        // Z 1 H -
        0x2D => unreachable,
        // LD L,d8
        // 2  8
        // - - - -
        0x2E => unreachable,
        // CPL
        // 1  4
        // - 1 1 -
        0x2F => unreachable,
        // JR NC,r8
        // 2  12/8
        // - - - -
        0x30 => unreachable,
        // LD SP,d16
        // 3  12
        // - - - -
        0x31 => unreachable,
        // LD (HL-),A
        // 1  8
        // - - - -
        0x32 => unreachable,
        // INC SP
        // 1  8
        // - - - -
        0x33 => unreachable,
        // INC (HL)
        // 1  12
        // Z 0 H -
        0x34 => unreachable,
        // DEC (HL)
        // 1  12
        // Z 1 H -
        0x35 => unreachable,
        // LD (HL),d8
        // 2  12
        // - - - -
        0x36 => unreachable,
        // SCF
        // 1  4
        // - 0 0 1
        0x37 => unreachable,
        // JR C,r8
        // 2  12/8
        // - - - -
        0x38 => unreachable,
        // ADD HL,SP
        // 1  8
        // - 0 H C
        0x39 => unreachable,
        // LD A,(HL-)
        // 1  8
        // - - - -
        0x3A => unreachable,
        // DEC SP
        // 1  8
        // - - - -
        0x3B => unreachable,
        // INC A
        // 1  4
        // Z 0 H -
        0x3C => unreachable,
        // DEC A
        // 1  4
        // Z 1 H -
        0x3D => unreachable,
        // LD A,d8
        // 2  8
        // - - - -
        0x3E => unreachable,
        // CCF
        // 1  4
        // - 0 0 C
        0x3F => unreachable,
        // LD B,B
        // 1  4
        // - - - -
        0x40 => unreachable,
        // LD B,C
        // 1  4
        // - - - -
        0x41 => unreachable,
        // LD B,D
        // 1  4
        // - - - -
        0x42 => unreachable,
        // LD B,E
        // 1  4
        // - - - -
        0x43 => unreachable,
        // LD B,H
        // 1  4
        // - - - -
        0x44 => unreachable,
        // LD B,L
        // 1  4
        // - - - -
        0x45 => unreachable,
        // LD B,(HL)
        // 1  8
        // - - - -
        0x46 => unreachable,
        // LD B,A
        // 1  4
        // - - - -
        0x47 => unreachable,
        // LD C,B
        // 1  4
        // - - - -
        0x48 => unreachable,
        // LD C,C
        // 1  4
        // - - - -
        0x49 => unreachable,
        // LD C,D
        // 1  4
        // - - - -
        0x4A => unreachable,
        // LD C,E
        // 1  4
        // - - - -
        0x4B => unreachable,
        // LD C,H
        // 1  4
        // - - - -
        0x4C => unreachable,
        // LD C,L
        // 1  4
        // - - - -
        0x4D => unreachable,
        // LD C,(HL)
        // 1  8
        // - - - -
        0x4E => unreachable,
        // LD C,A
        // 1  4
        // - - - -
        0x4F => unreachable,
        // LD D,B
        // 1  4
        // - - - -
        0x50 => unreachable,
        // LD D,C
        // 1  4
        // - - - -
        0x51 => unreachable,
        // LD D,D
        // 1  4
        // - - - -
        0x52 => unreachable,
        // LD D,E
        // 1  4
        // - - - -
        0x53 => unreachable,
        // LD D,H
        // 1  4
        // - - - -
        0x54 => unreachable,
        // LD D,L
        // 1  4
        // - - - -
        0x55 => unreachable,
        // LD D,(HL)
        // 1  8
        // - - - -
        0x56 => unreachable,
        // LD D,A
        // 1  4
        // - - - -
        0x57 => unreachable,
        // LD E,B
        // 1  4
        // - - - -
        0x58 => unreachable,
        // LD E,C
        // 1  4
        // - - - -
        0x59 => unreachable,
        // LD E,D
        // 1  4
        // - - - -
        0x5A => unreachable,
        // LD E,E
        // 1  4
        // - - - -
        0x5B => unreachable,
        // LD E,H
        // 1  4
        // - - - -
        0x5C => unreachable,
        // LD E,L
        // 1  4
        // - - - -
        0x5D => unreachable,
        // LD E,(HL)
        // 1  8
        // - - - -
        0x5E => unreachable,
        // LD E,A
        // 1  4
        // - - - -
        0x5F => unreachable,
        // LD H,B
        // 1  4
        // - - - -
        0x60 => unreachable,
        // LD H,C
        // 1  4
        // - - - -
        0x61 => unreachable,
        // LD H,D
        // 1  4
        // - - - -
        0x62 => unreachable,
        // LD H,E
        // 1  4
        // - - - -
        0x63 => unreachable,
        // LD H,H
        // 1  4
        // - - - -
        0x64 => unreachable,
        // LD H,L
        // 1  4
        // - - - -
        0x65 => unreachable,
        // LD H,(HL)
        // 1  8
        // - - - -
        0x66 => unreachable,
        // LD H,A
        // 1  4
        // - - - -
        0x67 => unreachable,
        // LD L,B
        // 1  4
        // - - - -
        0x68 => unreachable,
        // LD L,C
        // 1  4
        // - - - -
        0x69 => unreachable,
        // LD L,D
        // 1  4
        // - - - -
        0x6A => unreachable,
        // LD L,E
        // 1  4
        // - - - -
        0x6B => unreachable,
        // LD L,H
        // 1  4
        // - - - -
        0x6C => unreachable,
        // LD L,L
        // 1  4
        // - - - -
        0x6D => unreachable,
        // LD L,(HL)
        // 1  8
        // - - - -
        0x6E => unreachable,
        // LD L,A
        // 1  4
        // - - - -
        0x6F => unreachable,
        // LD (HL),B
        // 1  8
        // - - - -
        0x70 => unreachable,
        // LD (HL),C
        // 1  8
        // - - - -
        0x71 => unreachable,
        // LD (HL),D
        // 1  8
        // - - - -
        0x72 => unreachable,
        // LD (HL),E
        // 1  8
        // - - - -
        0x73 => unreachable,
        // LD (HL),H
        // 1  8
        // - - - -
        0x74 => unreachable,
        // LD (HL),L
        // 1  8
        // - - - -
        0x75 => unreachable,
        // HALT
        // 1  4
        // - - - -
        0x76 => unreachable,
        // LD (HL),A
        // 1  8
        // - - - -
        0x77 => unreachable,
        // LD A,B
        // 1  4
        // - - - -
        0x78 => unreachable,
        // LD A,C
        // 1  4
        // - - - -
        0x79 => unreachable,
        // LD A,D
        // 1  4
        // - - - -
        0x7A => unreachable,
        // LD A,E
        // 1  4
        // - - - -
        0x7B => unreachable,
        // LD A,H
        // 1  4
        // - - - -
        0x7C => unreachable,
        // LD A,L
        // 1  4
        // - - - -
        0x7D => unreachable,
        // LD A,(HL)
        // 1  8
        // - - - -
        0x7E => unreachable,
        // LD A,A
        // 1  4
        // - - - -
        0x7F => unreachable,
        // ADD A,B
        // 1  4
        // Z 0 H C
        0x80 => unreachable,
        // ADD A,C
        // 1  4
        // Z 0 H C
        0x81 => unreachable,
        // ADD A,D
        // 1  4
        // Z 0 H C
        0x82 => unreachable,
        // ADD A,E
        // 1  4
        // Z 0 H C
        0x83 => unreachable,
        // ADD A,H
        // 1  4
        // Z 0 H C
        0x84 => unreachable,
        // ADD A,L
        // 1  4
        // Z 0 H C
        0x85 => unreachable,
        // ADD A,(HL)
        // 1  8
        // Z 0 H C
        0x86 => unreachable,
        // ADD A,A
        // 1  4
        // Z 0 H C
        0x87 => unreachable,
        // ADC A,B
        // 1  4
        // Z 0 H C
        0x88 => unreachable,
        // ADC A,C
        // 1  4
        // Z 0 H C
        0x89 => unreachable,
        // ADC A,D
        // 1  4
        // Z 0 H C
        0x8A => unreachable,
        // ADC A,E
        // 1  4
        // Z 0 H C
        0x8B => unreachable,
        // ADC A,H
        // 1  4
        // Z 0 H C
        0x8C => unreachable,
        // ADC A,L
        // 1  4
        // Z 0 H C
        0x8D => unreachable,
        // ADC A,(HL)
        // 1  8
        // Z 0 H C
        0x8E => unreachable,
        // ADC A,A
        // 1  4
        // Z 0 H C
        0x8F => unreachable,
        // SUB B
        // 1  4
        // Z 1 H C
        0x90 => unreachable,
        // SUB C
        // 1  4
        // Z 1 H C
        0x91 => unreachable,
        // SUB D
        // 1  4
        // Z 1 H C
        0x92 => unreachable,
        // SUB E
        // 1  4
        // Z 1 H C
        0x93 => unreachable,
        // SUB H
        // 1  4
        // Z 1 H C
        0x94 => unreachable,
        // SUB L
        // 1  4
        // Z 1 H C
        0x95 => unreachable,
        // SUB (HL)
        // 1  8
        // Z 1 H C
        0x96 => unreachable,
        // SUB A
        // 1  4
        // Z 1 H C
        0x97 => unreachable,
        // SBC A,B
        // 1  4
        // Z 1 H C
        0x98 => unreachable,
        // SBC A,C
        // 1  4
        // Z 1 H C
        0x99 => unreachable,
        // SBC A,D
        // 1  4
        // Z 1 H C
        0x9A => unreachable,
        // SBC A,E
        // 1  4
        // Z 1 H C
        0x9B => unreachable,
        // SBC A,H
        // 1  4
        // Z 1 H C
        0x9C => unreachable,
        // SBC A,L
        // 1  4
        // Z 1 H C
        0x9D => unreachable,
        // SBC A,(HL)
        // 1  8
        // Z 1 H C
        0x9E => unreachable,
        // SBC A,A
        // 1  4
        // Z 1 H C
        0x9F => unreachable,
        // AND B
        // 1  4
        // Z 0 1 0
        0xA0 => unreachable,
        // AND C
        // 1  4
        // Z 0 1 0
        0xA1 => unreachable,
        // AND D
        // 1  4
        // Z 0 1 0
        0xA2 => unreachable,
        // AND E
        // 1  4
        // Z 0 1 0
        0xA3 => unreachable,
        // AND H
        // 1  4
        // Z 0 1 0
        0xA4 => unreachable,
        // AND L
        // 1  4
        // Z 0 1 0
        0xA5 => unreachable,
        // AND (HL)
        // 1  8
        // Z 0 1 0
        0xA6 => unreachable,
        // AND A
        // 1  4
        // Z 0 1 0
        0xA7 => unreachable,
        // XOR B
        // 1  4
        // Z 0 0 0
        0xA8 => unreachable,
        // XOR C
        // 1  4
        // Z 0 0 0
        0xA9 => unreachable,
        // XOR D
        // 1  4
        // Z 0 0 0
        0xAA => unreachable,
        // XOR E
        // 1  4
        // Z 0 0 0
        0xAB => unreachable,
        // XOR H
        // 1  4
        // Z 0 0 0
        0xAC => unreachable,
        // XOR L
        // 1  4
        // Z 0 0 0
        0xAD => unreachable,
        // XOR (HL)
        // 1  8
        // Z 0 0 0
        0xAE => unreachable,
        // XOR A
        // 1  4
        // Z 0 0 0
        0xAF => unreachable,
        // OR B
        // 1  4
        // Z 0 0 0
        0xB0 => unreachable,
        // OR C
        // 1  4
        // Z 0 0 0
        0xB1 => unreachable,
        // OR D
        // 1  4
        // Z 0 0 0
        0xB2 => unreachable,
        // OR E
        // 1  4
        // Z 0 0 0
        0xB3 => unreachable,
        // OR H
        // 1  4
        // Z 0 0 0
        0xB4 => unreachable,
        // OR L
        // 1  4
        // Z 0 0 0
        0xB5 => unreachable,
        // OR (HL)
        // 1  8
        // Z 0 0 0
        0xB6 => unreachable,
        // OR A
        // 1  4
        // Z 0 0 0
        0xB7 => unreachable,
        // CP B
        // 1  4
        // Z 1 H C
        0xB8 => unreachable,
        // CP C
        // 1  4
        // Z 1 H C
        0xB9 => unreachable,
        // CP D
        // 1  4
        // Z 1 H C
        0xBA => unreachable,
        // CP E
        //1  4
        // Z 1 H C
        0xBB => unreachable,
        // CP H
        // 1  4
        // Z 1 H C
        0xBC => unreachable,
        // CP L
        // 1  4
        // Z 1 H C
        0xBD => unreachable,
        // CP (HL)
        // 1  8
        // Z 1 H C
        0xBE => unreachable,
        // CP A
        // 1  4
        // Z 1 H C
        0xBF => unreachable,
        // RET NZ
        // 1  20/8
        // - - - -
        0xC0 => unreachable,
        // POP BC
        // 1  12
        // - - - -
        0xC1 => unreachable,
        // JP NZ,a16
        // 3  16/12
        // - - - -
        0xC2 => unreachable,
        // JP a16
        // 3  16
        // - - - -
        0xC3 => instruction.jp(a16),
        // CALL NZ,a16
        // 3  24/12
        // - - - -
        0xC4 => unreachable,
        // PUSH BC
        // 1  16
        // - - - -
        0xC5 => unreachable,
        // ADD A,d8
        // 2  8
        // Z 0 H C
        0xC6 => unreachable,
        // RST 00H
        // 1  16
        // - - - -
        0xC7 => unreachable,
        // RET Z
        // 1  20/8
        // - - - -
        0xC8 => unreachable,
        // RET
        // 1  16
        // - - - -
        0xC9 => unreachable,
        // JP Z,a16
        // 3  16/12
        // - - - -
        0xCA => unreachable,
        0xCB => switch (cb_insn) {
            //
            0x00 => unreachable,
            //
            0x01 => unreachable,
            //
            0x02 => unreachable,
            //
            0x03 => unreachable,
            //
            0x04 => unreachable,
            //
            0x05 => unreachable,
            //
            0x06 => unreachable,
            //
            0x07 => unreachable,
            //
            0x08 => unreachable,
            //
            0x09 => unreachable,
            //
            0x0A => unreachable,
            //
            0x0B => unreachable,
            //
            0x0C => unreachable,
            //
            0x0D => unreachable,
            //
            0x0E => unreachable,
            //
            0x0F => unreachable,
            //
            0x10 => unreachable,
            //
            0x11 => unreachable,
            //
            0x12 => unreachable,
            //
            0x13 => unreachable,
            //
            0x14 => unreachable,
            //
            0x15 => unreachable,
            //
            0x16 => unreachable,
            //
            0x17 => unreachable,
            //
            0x18 => unreachable,
            //
            0x19 => unreachable,
            //
            0x1A => unreachable,
            //
            0x1B => unreachable,
            //
            0x1C => unreachable,
            //
            0x1D => unreachable,
            //
            0x1E => unreachable,
            //
            0x1F => unreachable,
            //
            0x20 => unreachable,
            //
            0x21 => unreachable,
            //
            0x22 => unreachable,
            //
            0x23 => unreachable,
            //
            0x24 => unreachable,
            //
            0x25 => unreachable,
            //
            0x26 => unreachable,
            //
            0x27 => unreachable,
            //
            0x28 => unreachable,
            //
            0x29 => unreachable,
            //
            0x2A => unreachable,
            //
            0x2B => unreachable,
            //
            0x2C => unreachable,
            //
            0x2D => unreachable,
            //
            0x2E => unreachable,
            //
            0x2F => unreachable,
            //
            0x30 => unreachable,
            //
            0x31 => unreachable,
            //
            0x32 => unreachable,
            //
            0x33 => unreachable,
            //
            0x34 => unreachable,
            //
            0x35 => unreachable,
            //
            0x36 => unreachable,
            //
            0x37 => unreachable,
            //
            0x38 => unreachable,
            //
            0x39 => unreachable,
            //
            0x3A => unreachable,
            //
            0x3B => unreachable,
            //
            0x3C => unreachable,
            //
            0x3D => unreachable,
            //
            0x3E => unreachable,
            //
            0x3F => unreachable,
            //
            0x40 => unreachable,
            //
            0x41 => unreachable,
            //
            0x42 => unreachable,
            //
            0x43 => unreachable,
            //
            0x44 => unreachable,
            //
            0x45 => unreachable,
            //
            0x46 => unreachable,
            //
            0x47 => unreachable,
            //
            0x48 => unreachable,
            //
            0x49 => unreachable,
            //
            0x4A => unreachable,
            //
            0x4B => unreachable,
            //
            0x4C => unreachable,
            //
            0x4D => unreachable,
            //
            0x4E => unreachable,
            //
            0x4F => unreachable,
            //
            0x50 => unreachable,
            //
            0x51 => unreachable,
            //
            0x52 => unreachable,
            //
            0x53 => unreachable,
            //
            0x54 => unreachable,
            //
            0x55 => unreachable,
            //
            0x56 => unreachable,
            //
            0x57 => unreachable,
            //
            0x58 => unreachable,
            //
            0x59 => unreachable,
            //
            0x5A => unreachable,
            //
            0x5B => unreachable,
            //
            0x5C => unreachable,
            //
            0x5D => unreachable,
            //
            0x5E => unreachable,
            //
            0x5F => unreachable,
            //
            0x60 => unreachable,
            //
            0x61 => unreachable,
            //
            0x62 => unreachable,
            //
            0x63 => unreachable,
            //
            0x64 => unreachable,
            //
            0x65 => unreachable,
            //
            0x66 => unreachable,
            //
            0x67 => unreachable,
            //
            0x68 => unreachable,
            //
            0x69 => unreachable,
            //
            0x6A => unreachable,
            //
            0x6B => unreachable,
            //
            0x6C => unreachable,
            //
            0x6D => unreachable,
            //
            0x6E => unreachable,
            //
            0x6F => unreachable,
            //
            0x70 => unreachable,
            //
            0x71 => unreachable,
            //
            0x72 => unreachable,
            //
            0x73 => unreachable,
            //
            0x74 => unreachable,
            //
            0x75 => unreachable,
            //
            0x76 => unreachable,
            //
            0x77 => unreachable,
            //
            0x78 => unreachable,
            //
            0x79 => unreachable,
            //
            0x7A => unreachable,
            //
            0x7B => unreachable,
            //
            0x7C => unreachable,
            //
            0x7D => unreachable,
            //
            0x7E => unreachable,
            //
            0x7F => unreachable,
            //
            0x80 => unreachable,
            //
            0x81 => unreachable,
            //
            0x82 => unreachable,
            //
            0x83 => unreachable,
            //
            0x84 => unreachable,
            //
            0x85 => unreachable,
            //
            0x86 => unreachable,
            //
            0x87 => unreachable,
            //
            0x88 => unreachable,
            //
            0x89 => unreachable,
            //
            0x8A => unreachable,
            //
            0x8B => unreachable,
            //
            0x8C => unreachable,
            //
            0x8D => unreachable,
            //
            0x8E => unreachable,
            //
            0x8F => unreachable,
            //
            0x90 => unreachable,
            //
            0x91 => unreachable,
            //
            0x92 => unreachable,
            //
            0x93 => unreachable,
            //
            0x94 => unreachable,
            //
            0x95 => unreachable,
            //
            0x96 => unreachable,
            //
            0x97 => unreachable,
            //
            0x98 => unreachable,
            //
            0x99 => unreachable,
            //
            0x9A => unreachable,
            //
            0x9B => unreachable,
            //
            0x9C => unreachable,
            //
            0x9D => unreachable,
            //
            0x9E => unreachable,
            //
            0x9F => unreachable,
            //
            0xA0 => unreachable,
            //
            0xA1 => unreachable,
            //
            0xA2 => unreachable,
            //
            0xA3 => unreachable,
            //
            0xA4 => unreachable,
            //
            0xA5 => unreachable,
            //
            0xA6 => unreachable,
            //
            0xA7 => unreachable,
            //
            0xA8 => unreachable,
            //
            0xA9 => unreachable,
            //
            0xAA => unreachable,
            //
            0xAB => unreachable,
            //
            0xAC => unreachable,
            //
            0xAD => unreachable,
            //
            0xAE => unreachable,
            //
            0xAF => unreachable,
            //
            0xB0 => unreachable,
            //
            0xB1 => unreachable,
            //
            0xB2 => unreachable,
            //
            0xB3 => unreachable,
            //
            0xB4 => unreachable,
            //
            0xB5 => unreachable,
            //
            0xB6 => unreachable,
            //
            0xB7 => unreachable,
            //
            0xB8 => unreachable,
            //
            0xB9 => unreachable,
            //
            0xBA => unreachable,
            //
            0xBB => unreachable,
            //
            0xBC => unreachable,
            //
            0xBD => unreachable,
            //
            0xBE => unreachable,
            //
            0xBF => unreachable,
            //
            0xC0 => unreachable,
            //
            0xC1 => unreachable,
            //
            0xC2 => unreachable,
            //
            0xC3 => unreachable,
            //
            0xC4 => unreachable,
            //
            0xC5 => unreachable,
            //
            0xC6 => unreachable,
            //
            0xC7 => unreachable,
            //
            0xC8 => unreachable,
            //
            0xC9 => unreachable,
            //
            0xCA => unreachable,
            //
            0xCB => unreachable,
            //
            0xCC => unreachable,
            //
            0xCD => unreachable,
            //
            0xCE => unreachable,
            //
            0xCF => unreachable,
            //
            0xD0 => unreachable,
            //
            0xD1 => unreachable,
            //
            0xD2 => unreachable,
            //
            0xD3 => unreachable,
            //
            0xD4 => unreachable,
            //
            0xD5 => unreachable,
            //
            0xD6 => unreachable,
            //
            0xD7 => unreachable,
            //
            0xD8 => unreachable,
            //
            0xD9 => unreachable,
            //
            0xDA => unreachable,
            //
            0xDB => unreachable,
            //
            0xDC => unreachable,
            //
            0xDD => unreachable,
            //
            0xDE => unreachable,
            //
            0xDF => unreachable,
            //
            0xE0 => unreachable,
            //
            0xE1 => unreachable,
            //
            0xE2 => unreachable,
            //
            0xE3 => unreachable,
            //
            0xE4 => unreachable,
            //
            0xE5 => unreachable,
            //
            0xE6 => unreachable,
            //
            0xE7 => unreachable,
            //
            0xE8 => unreachable,
            //
            0xE9 => unreachable,
            //
            0xEA => unreachable,
            //
            0xEB => unreachable,
            //
            0xEC => unreachable,
            //
            0xED => unreachable,
            //
            0xEE => unreachable,
            //
            0xEF => unreachable,
            //
            0xF0 => unreachable,
            //
            0xF1 => unreachable,
            //
            0xF2 => unreachable,
            //
            0xF3 => unreachable,
            //
            0xF4 => unreachable,
            //
            0xF5 => unreachable,
            //
            0xF6 => unreachable,
            //
            0xF7 => unreachable,
            //
            0xF8 => unreachable,
            //
            0xF9 => unreachable,
            //
            0xFA => unreachable,
            //
            0xFB => unreachable,
            //
            0xFC => unreachable,
            //
            0xFD => unreachable,
            //
            0xFE => unreachable,
            //
            0xFF => unreachable,
        },
        // CALL Z,a16
        // 3  24/12
        // - - - -
        0xCC => unreachable,
        // CALL a16
        // 3  24
        // - - - -
        0xCD => unreachable,
        // ADC A,d8
        // 2  8
        // Z 0 H C
        0xCE => unreachable,
        // RST 08H
        // 1  16
        // - - - -
        0xCF => unreachable,
        // RET NC
        // 1  20/8
        // - - - -
        0xD0 => unreachable,
        // POP DE
        // 1  12
        // - - - -
        0xD1 => unreachable,
        // JP NC,a16
        // 3  16/12
        // - - - -
        0xD2 => unreachable,
        // No instruction
        0xD3 => unreachable,
        // 	CALL NC,a16
        // 3  24/12
        // - - - -
        0xD4 => unreachable,
        // PUSH DE
        // 1  16
        // - - - -
        0xD5 => unreachable,
        // SUB d8
        // 2  8
        // Z 1 H C
        0xD6 => unreachable,
        // RST 10H
        // 1  16
        // - - - -
        0xD7 => unreachable,
        // RET C
        // 1  20/8
        // - - - -
        0xD8 => unreachable,
        // RETI
        // 1  16
        // - - - -
        0xD9 => unreachable,
        // JP C,a16
        // 3  16/12
        // - - - -
        0xDA => unreachable,
        // No instruction
        0xDB => unreachable,
        // CALL C,a16
        // 3  24/12
        // - - - -
        0xDC => unreachable,
        // No instruction
        0xDD => unreachable,
        // SBC A,d8
        // 2  8
        // Z 1 H C
        0xDE => unreachable,
        // RST 18H
        // 1  16
        // - - - -
        0xDF => unreachable,
        // LDH (a8),A
        // 2  12
        // - - - -
        0xE0 => unreachable,
        // POP HL
        // 1  12
        // - - - -
        0xE1 => unreachable,
        // LD (C),A
        // 2  8
        // - - - -
        0xE2 => unreachable,
        // No instruction
        0xE3 => unreachable,
        // No instruction
        0xE4 => unreachable,
        // PUSH HL
        // 1  16
        // - - - -
        0xE5 => unreachable,
        // AND d8
        // 2  8
        // Z 0 1 0
        0xE6 => unreachable,
        // RST 20H
        // 1  16
        // - - - -
        0xE7 => unreachable,
        // ADD SP,r8
        // 2  16
        // 0 0 H C
        0xE8 => unreachable,
        // JP (HL)
        // 1  4
        // - - - -
        0xE9 => unreachable,
        // LD (a16),A
        // 3  16
        // - - - -
        0xEA => unreachable,
        // No instruction
        0xEB => unreachable,
        // No instruction
        0xEC => unreachable,
        // No instruction
        0xED => unreachable,
        // XOR d8
        // 2  8
        // Z 0 0 0
        0xEE => unreachable,
        // RST 28H
        // 1  16
        // - - - -
        0xEF => unreachable,
        // LDH A,(a8)
        // 2  12
        // - - - -
        0xF0 => unreachable,
        // POP AF
        // 1  12
        // Z N H C
        0xF1 => unreachable,
        // LD A,(C)
        // 2  8
        // - - - -
        0xF2 => unreachable,
        // DI
        // 1  4
        // - - - -
        0xF3 => unreachable,
        // No instruction
        0xF4 => unreachable,
        // PUSH AF
        // 1  16
        // - - - -
        0xF5 => unreachable,
        // OR d8
        // 2  8
        // Z 0 0 0
        0xF6 => unreachable,
        // RST 30H
        // 1  16
        // - - - -
        0xF7 => unreachable,
        // LD HL,SP+r8
        // 2  12
        // 0 0 H C
        0xF8 => unreachable,
        // LD SP,HL
        // 1  8
        // - - - -
        0xF9 => unreachable,
        // LD A,(a16)
        // 3  16
        // - - - -
        0xFA => unreachable,
        // EI
        // 1  4
        // - - - -
        0xFB => unreachable,
        // No instruction
        0xFC => unreachable,
        // No instruction
        0xFD => unreachable,
        // CP d8
        // 2  8
        // Z 1 H C
        0xFE => unreachable,
        // RST 38H
        // 1  16
        // - - - -
        0xFF => unreachable,
    };

    std.debug.print("This instruction took 0x{x} cycles\n", .{cycles});
}
