const std = @import("std");
const st = @import("state.zig");
const regs = st.Regs;
const cc = st.Cond;
const insn = @import("instructions.zig");

// Execute the instruction pointed to by the PC register
pub fn execute(state: *st.State) void {
    executeAt(state.getReg(st.Regs.PC), state);
}

// Execute the instruction pointed to by address
pub fn executeAt(address: u16, state: *st.State) void {
    //std.debug.print("Executing insn at 0x{x}, PC: 0x{x}, first insn byte 0x{x}\n", .{ address, state.getReg(st.Regs.PC), rom_data[address] });

    const instruction: insn.Instruction = insn.Instruction.init(state);
    const cb_insn: u8 = state.readUnsignedByte(address + 1);

    const d8: u8 = state.readUnsignedByte(address + 1);
    const d16: u16 = state.readUnsignedWord(address + 1);

    //const a8: u8 = state.readUnignedByte(address + 1);
    const a16: u16 = state.readUnsignedWord(address + 1);

    const r8: i8 = state.readSignedByte(address + 1);

    //std.debug.print("cb_insn: 0x{x} d8: 0x{x} d16: 0x{x} a8: 0x{x} a16: 0x{x} r8: 0x{x}\n", .{ cb_insn, d8, d16, a8, a16, r8 });

    // See https://www.pastraiser.com/cpu/gameboy/gameboy_opcodes.html
    const cycles: u8 = switch (state.memory[address]) {
        // NOP
        // 1  4
        // - - - -
        0x00 => instruction.nop(),
        // LD BC,d16
        // 3  12
        // - - - -
        0x01 => instruction.ldImm16(regs.BC, d16),
        // LD (BC),A
        // 1  8
        // - - - -
        0x02 => instruction.ldReg(regs.BC, regs.A, true, false, false, false),
        // INC BC
        // 1  8
        // - - - -
        0x03 => instruction.inc(regs.BC, false),
        // INC B
        // 1  4
        // Z 0 H -
        0x04 => instruction.inc(regs.B, false),
        // DEC B
        // 1  4
        // Z 1 H -
        0x05 => instruction.dec(regs.B, false),
        // LD B,d8
        // 2  8
        // - - - -
        0x06 => instruction.ldImm8(regs.B, d8, false),
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
        0x0B => instruction.dec(regs.BC, false),
        // INC C
        // 1  4
        // Z 0 H -
        0x0C => instruction.inc(regs.C, false),
        // DEC C
        // 1  4
        // Z 1 H -
        0x0D => instruction.dec(regs.C, false),
        // LD C,d8
        // 2  8
        // - - - -
        0x0E => instruction.ldImm8(regs.C, d8, false),
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
        0x11 => instruction.ldImm16(regs.DE, d16),
        // LD (DE),A
        // 1  8
        // - - - -
        0x12 => instruction.ldReg(regs.DE, regs.A, true, false, false, false),
        // INC DE
        // 1  8
        // - - - -
        0x13 => instruction.inc(regs.DE, false),
        // INC D
        // 1  4
        // Z 0 H -
        0x14 => instruction.inc(regs.D, false),
        // DEC D
        // 1  4
        // Z 1 H -
        0x15 => instruction.dec(regs.D, false),
        // LD D,d8
        // 2  8
        // - - - -
        0x16 => instruction.ldImm8(regs.D, d8, false),
        // RLA
        // 1  4
        // 0 0 0 C
        0x17 => unreachable,
        // JR r8
        // 2  12
        // - - - -
        0x18 => instruction.jr(r8),
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
        0x1B => instruction.dec(regs.DE, false),
        // INC E
        // 1  4
        // Z 0 H -
        0x1C => instruction.inc(regs.E, false),
        // DEC E
        // 1  4
        // Z 1 H -
        0x1D => instruction.dec(regs.E, false),
        // LD E,d8
        // 2  8
        // - - - -
        0x1E => instruction.ldImm8(regs.E, d8, false),
        // RRA
        // 1  4
        // 0 0 0 C
        0x1F => unreachable,
        // JR NZ,r8
        // 2  12/8
        // - - - -
        0x20 => instruction.jrCond(cc.NZ, r8),
        // LD HL,d16
        // 3  12
        // - - - -
        0x21 => instruction.ldImm16(regs.HL, d16),
        // LD (HL+),A
        // 1  8
        // - - - -
        0x22 => instruction.ldReg(regs.HL, regs.A, true, false, true, false),
        // INC HL
        // 1  8
        // - - - -
        0x23 => instruction.inc(regs.HL, false),
        // INC H
        // 1  4
        // Z 0 H -
        0x24 => instruction.inc(regs.H, false),
        // DEC H
        // 1  4
        // Z 1 H -
        0x25 => instruction.dec(regs.H, false),
        // LD H,d8
        // 2  8
        // - - - -
        0x26 => instruction.ldImm8(regs.H, d8, false),
        // DAA
        // 1  4
        // Z - 0 C
        0x27 => unreachable,
        // JR Z,r8
        // 2  12/8
        // - - - -
        0x28 => instruction.jrCond(cc.Z, r8),
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
        0x2B => instruction.dec(regs.HL, false),
        // INC L
        // 1  4
        // Z 0 H -
        0x2C => instruction.inc(regs.L, false),
        // DEC L
        // 1  4
        // Z 1 H -
        0x2D => instruction.dec(regs.L, false),
        // LD L,d8
        // 2  8
        // - - - -
        0x2E => instruction.ldImm8(regs.L, d8, false),
        // CPL
        // 1  4
        // - 1 1 -
        0x2F => unreachable,
        // JR NC,r8
        // 2  12/8
        // - - - -
        0x30 => instruction.jrCond(cc.NC, r8),
        // LD SP,d16
        // 3  12
        // - - - -
        0x31 => instruction.ldImm16(regs.SP, d16),
        // LD (HL-),A
        // 1  8
        // - - - -
        0x32 => instruction.ldReg(regs.HL, regs.A, true, false, false, true),
        // INC SP
        // 1  8
        // - - - -
        0x33 => instruction.inc(regs.SP, false),
        // INC (HL)
        // 1  12
        // Z 0 H -
        0x34 => instruction.inc(regs.HL, true),
        // DEC (HL)
        // 1  12
        // Z 1 H -
        0x35 => instruction.dec(regs.HL, true),
        // LD (HL),d8
        // 2  12
        // - - - -
        0x36 => instruction.ldImm8(regs.HL, d8, true),
        // SCF
        // 1  4
        // - 0 0 1
        0x37 => unreachable,
        // JR C,r8
        // 2  12/8
        // - - - -
        0x38 => instruction.jrCond(cc.C, r8),
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
        0x3B => instruction.dec(regs.SP, false),
        // INC A
        // 1  4
        // Z 0 H -
        0x3C => instruction.inc(regs.A, false),
        // DEC A
        // 1  4
        // Z 1 H -
        0x3D => instruction.dec(regs.A, false),
        // LD A,d8
        // 2  8
        // - - - -
        0x3E => instruction.ldImm8(regs.A, d8, false),
        // CCF
        // 1  4
        // - 0 0 C
        0x3F => unreachable,
        // LD B,B
        // 1  4
        // - - - -
        0x40 => instruction.ldReg(regs.B, regs.B, false, false, false, false),
        // LD B,C
        // 1  4
        // - - - -
        0x41 => instruction.ldReg(regs.B, regs.C, false, false, false, false),
        // LD B,D
        // 1  4
        // - - - -
        0x42 => instruction.ldReg(regs.B, regs.D, false, false, false, false),
        // LD B,E
        // 1  4
        // - - - -
        0x43 => instruction.ldReg(regs.B, regs.E, false, false, false, false),
        // LD B,H
        // 1  4
        // - - - -
        0x44 => instruction.ldReg(regs.B, regs.H, false, false, false, false),
        // LD B,L
        // 1  4
        // - - - -
        0x45 => instruction.ldReg(regs.B, regs.L, false, false, false, false),
        // LD B,(HL)
        // 1  8
        // - - - -
        0x46 => instruction.ldReg(regs.B, regs.HL, false, true, false, false),
        // LD B,A
        // 1  4
        // - - - -
        0x47 => instruction.ldReg(regs.B, regs.A, false, false, false, false),
        // LD C,B
        // 1  4
        // - - - -
        0x48 => instruction.ldReg(regs.C, regs.B, false, false, false, false),
        // LD C,C
        // 1  4
        // - - - -
        0x49 => instruction.ldReg(regs.C, regs.C, false, false, false, false),
        // LD C,D
        // 1  4
        // - - - -
        0x4A => instruction.ldReg(regs.C, regs.D, false, false, false, false),
        // LD C,E
        // 1  4
        // - - - -
        0x4B => instruction.ldReg(regs.C, regs.E, false, false, false, false),
        // LD C,H
        // 1  4
        // - - - -
        0x4C => instruction.ldReg(regs.C, regs.H, false, false, false, false),
        // LD C,L
        // 1  4
        // - - - -
        0x4D => instruction.ldReg(regs.C, regs.L, false, false, false, false),
        // LD C,(HL)
        // 1  8
        // - - - -
        0x4E => instruction.ldReg(regs.C, regs.HL, false, true, false, false),
        // LD C,A
        // 1  4
        // - - - -
        0x4F => instruction.ldReg(regs.C, regs.A, false, false, false, false),
        // LD D,B
        // 1  4
        // - - - -
        0x50 => instruction.ldReg(regs.D, regs.B, false, false, false, false),
        // LD D,C
        // 1  4
        // - - - -
        0x51 => instruction.ldReg(regs.D, regs.C, false, false, false, false),
        // LD D,D
        // 1  4
        // - - - -
        0x52 => instruction.ldReg(regs.D, regs.D, false, false, false, false),
        // LD D,E
        // 1  4
        // - - - -
        0x53 => instruction.ldReg(regs.D, regs.E, false, false, false, false),
        // LD D,H
        // 1  4
        // - - - -
        0x54 => instruction.ldReg(regs.D, regs.H, false, false, false, false),
        // LD D,L
        // 1  4
        // - - - -
        0x55 => instruction.ldReg(regs.D, regs.L, false, false, false, false),
        // LD D,(HL)
        // 1  8
        // - - - -
        0x56 => instruction.ldReg(regs.D, regs.HL, false, true, false, false),
        // LD D,A
        // 1  4
        // - - - -
        0x57 => instruction.ldReg(regs.D, regs.A, false, false, false, false),
        // LD E,B
        // 1  4
        // - - - -
        0x58 => instruction.ldReg(regs.E, regs.B, false, false, false, false),
        // LD E,C
        // 1  4
        // - - - -
        0x59 => instruction.ldReg(regs.E, regs.C, false, false, false, false),
        // LD E,D
        // 1  4
        // - - - -
        0x5A => instruction.ldReg(regs.E, regs.D, false, false, false, false),
        // LD E,E
        // 1  4
        // - - - -
        0x5B => instruction.ldReg(regs.E, regs.E, false, false, false, false),
        // LD E,H
        // 1  4
        // - - - -
        0x5C => instruction.ldReg(regs.E, regs.H, false, false, false, false),
        // LD E,L
        // 1  4
        // - - - -
        0x5D => instruction.ldReg(regs.E, regs.L, false, false, false, false),
        // LD E,(HL)
        // 1  8
        // - - - -
        0x5E => instruction.ldReg(regs.E, regs.HL, false, true, false, false),
        // LD E,A
        // 1  4
        // - - - -
        0x5F => instruction.ldReg(regs.E, regs.A, false, false, false, false),
        // LD H,B
        // 1  4
        // - - - -
        0x60 => instruction.ldReg(regs.H, regs.B, false, false, false, false),
        // LD H,C
        // 1  4
        // - - - -
        0x61 => instruction.ldReg(regs.H, regs.C, false, false, false, false),
        // LD H,D
        // 1  4
        // - - - -
        0x62 => instruction.ldReg(regs.H, regs.D, false, false, false, false),
        // LD H,E
        // 1  4
        // - - - -
        0x63 => instruction.ldReg(regs.H, regs.E, false, false, false, false),
        // LD H,H
        // 1  4
        // - - - -
        0x64 => instruction.ldReg(regs.H, regs.H, false, false, false, false),
        // LD H,L
        // 1  4
        // - - - -
        0x65 => instruction.ldReg(regs.H, regs.L, false, false, false, false),
        // LD H,(HL)
        // 1  8
        // - - - -
        0x66 => instruction.ldReg(regs.H, regs.HL, false, true, false, false),
        // LD H,A
        // 1  4
        // - - - -
        0x67 => instruction.ldReg(regs.H, regs.A, false, false, false, false),
        // LD L,B
        // 1  4
        // - - - -
        0x68 => instruction.ldReg(regs.L, regs.B, false, false, false, false),
        // LD L,C
        // 1  4
        // - - - -
        0x69 => instruction.ldReg(regs.L, regs.C, false, false, false, false),
        // LD L,D
        // 1  4
        // - - - -
        0x6A => instruction.ldReg(regs.L, regs.D, false, false, false, false),
        // LD L,E
        // 1  4
        // - - - -
        0x6B => instruction.ldReg(regs.L, regs.E, false, false, false, false),
        // LD L,H
        // 1  4
        // - - - -
        0x6C => instruction.ldReg(regs.L, regs.H, false, false, false, false),
        // LD L,L
        // 1  4
        // - - - -
        0x6D => instruction.ldReg(regs.L, regs.L, false, false, false, false),
        // LD L,(HL)
        // 1  8
        // - - - -
        0x6E => instruction.ldReg(regs.L, regs.HL, false, true, false, false),
        // LD L,A
        // 1  4
        // - - - -
        0x6F => instruction.ldReg(regs.L, regs.A, false, false, false, false),
        // LD (HL),B
        // 1  8
        // - - - -
        0x70 => instruction.ldReg(regs.HL, regs.B, true, false, false, false),
        // LD (HL),C
        // 1  8
        // - - - -
        0x71 => instruction.ldReg(regs.HL, regs.C, true, false, false, false),
        // LD (HL),D
        // 1  8
        // - - - -
        0x72 => instruction.ldReg(regs.HL, regs.D, true, false, false, false),
        // LD (HL),E
        // 1  8
        // - - - -
        0x73 => instruction.ldReg(regs.HL, regs.E, true, false, false, false),
        // LD (HL),H
        // 1  8
        // - - - -
        0x74 => instruction.ldReg(regs.HL, regs.H, true, false, false, false),
        // LD (HL),L
        // 1  8
        // - - - -
        0x75 => instruction.ldReg(regs.HL, regs.L, true, false, false, false),
        // HALT
        // 1  4
        // - - - -
        0x76 => unreachable,
        // LD (HL),A
        // 1  8
        // - - - -
        0x77 => instruction.ldReg(regs.HL, regs.A, true, false, false, false),
        // LD A,B
        // 1  4
        // - - - -
        0x78 => instruction.ldReg(regs.A, regs.B, false, false, false, false),
        // LD A,C
        // 1  4
        // - - - -
        0x79 => instruction.ldReg(regs.A, regs.C, false, false, false, false),
        // LD A,D
        // 1  4
        // - - - -
        0x7A => instruction.ldReg(regs.A, regs.D, false, false, false, false),
        // LD A,E
        // 1  4
        // - - - -
        0x7B => instruction.ldReg(regs.A, regs.E, false, false, false, false),
        // LD A,H
        // 1  4
        // - - - -
        0x7C => instruction.ldReg(regs.A, regs.H, false, false, false, false),
        // LD A,L
        // 1  4
        // - - - -
        0x7D => instruction.ldReg(regs.A, regs.L, false, false, false, false),
        // LD A,(HL)
        // 1  8
        // - - - -
        0x7E => instruction.ldReg(regs.A, regs.HL, false, true, false, false),
        // LD A,A
        // 1  4
        // - - - -
        0x7F => instruction.ldReg(regs.A, regs.A, false, false, false, false),
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
        0xA8 => instruction.xor(regs.B, false),
        // XOR C
        // 1  4
        // Z 0 0 0
        0xA9 => instruction.xor(regs.C, false),
        // XOR D
        // 1  4
        // Z 0 0 0
        0xAA => instruction.xor(regs.D, false),
        // XOR E
        // 1  4
        // Z 0 0 0
        0xAB => instruction.xor(regs.E, false),
        // XOR H
        // 1  4
        // Z 0 0 0
        0xAC => instruction.xor(regs.H, false),
        // XOR L
        // 1  4
        // Z 0 0 0
        0xAD => instruction.xor(regs.L, false),
        // XOR (HL)
        // 1  8
        // Z 0 0 0
        0xAE => instruction.xor(regs.HL, true),
        // XOR A
        // 1  4
        // Z 0 0 0
        0xAF => instruction.xor(regs.A, false),
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
            // RLC B
            // 2  8
            // Z 0 0 C
            0x00 => unreachable,
            // RLC C
            // 2  8
            // Z 0 0 C
            0x01 => unreachable,
            // RLC D
            // 2  8
            // Z 0 0 C
            0x02 => unreachable,
            // RLC E
            // 2  8
            // Z 0 0 C
            0x03 => unreachable,
            // RLC H
            // 2  8
            // Z 0 0 C
            0x04 => unreachable,
            // RLC L
            // 2  8
            // Z 0 0 C
            0x05 => unreachable,
            // RLC (HL)
            // 2  16
            // Z 0 0 C
            0x06 => unreachable,
            // RLC A
            // 2  8
            // Z 0 0 C
            0x07 => unreachable,
            // RRC B
            // 2  8
            // Z 0 0 C
            0x08 => unreachable,
            // RRC C
            // 2  8
            // Z 0 0 C
            0x09 => unreachable,
            // RRC D
            // 2  8
            // Z 0 0 C
            0x0A => unreachable,
            // RRC E
            // 2  8
            // Z 0 0 C
            0x0B => unreachable,
            // RRC H
            // 2  8
            // Z 0 0 C
            0x0C => unreachable,
            // RRC L
            // 2  8
            // Z 0 0 C
            0x0D => unreachable,
            // RRC (HL)
            // 2  16
            // Z 0 0 C
            0x0E => unreachable,
            // RRC A
            // 2  8
            // Z 0 0 C
            0x0F => unreachable,
            // RL B
            // 2  8
            // Z 0 0 C
            0x10 => unreachable,
            // RL C
            // 2  8
            // Z 0 0 C
            0x11 => unreachable,
            // RL D
            // 2  8
            // Z 0 0 C
            0x12 => unreachable,
            // RL E
            // 2  8
            // Z 0 0 C
            0x13 => unreachable,
            // RL H
            // 2  8
            // Z 0 0 C
            0x14 => unreachable,
            // RL L
            // 2  8
            // Z 0 0 C
            0x15 => unreachable,
            // RL (HL)
            // 2  16
            // Z 0 0 C
            0x16 => unreachable,
            // RL A
            // 2  8
            // Z 0 0 C
            0x17 => unreachable,
            // RR B
            // 2  8
            // Z 0 0 C
            0x18 => unreachable,
            // RR C
            // 2  8
            // Z 0 0 C
            0x19 => unreachable,
            // RR D
            // 2  8
            // Z 0 0 C
            0x1A => unreachable,
            // RR E
            // 2  8
            // Z 0 0 C
            0x1B => unreachable,
            // RR H
            // 2  8
            // Z 0 0 C
            0x1C => unreachable,
            // RR L
            // 2  8
            // Z 0 0 C
            0x1D => unreachable,
            // RR (HL)
            // 2  16
            // Z 0 0 C
            0x1E => unreachable,
            // RR A
            // 2  8
            // Z 0 0 C
            0x1F => unreachable,
            // SLA B
            // 2  8
            // Z 0 0 C
            0x20 => unreachable,
            // SLA C
            // 2  8
            // Z 0 0 C
            0x21 => unreachable,
            // SLA D
            // 2  8
            // Z 0 0 C
            0x22 => unreachable,
            // SLA E
            // 2  8
            // Z 0 0 C
            0x23 => unreachable,
            // SLA H
            // 2  8
            // Z 0 0 C
            0x24 => unreachable,
            // SLA L
            // 2  8
            // Z 0 0 C
            0x25 => unreachable,
            // SLA (HL)
            // 2  16
            // Z 0 0 C
            0x26 => unreachable,
            // SLA A
            // 2  8
            // Z 0 0 C
            0x27 => unreachable,
            // SRA B
            // 2  8
            // Z 0 0 0
            0x28 => unreachable,
            // SRA C
            // 2  8
            // Z 0 0 0
            0x29 => unreachable,
            // SRA D
            // 2  8
            // Z 0 0 0
            0x2A => unreachable,
            // SRA E
            // 2  8
            // Z 0 0 0
            0x2B => unreachable,
            // SRA H
            // 2  8
            // Z 0 0 0
            0x2C => unreachable,
            // SRA L
            // 2  8
            // Z 0 0 0
            0x2D => unreachable,
            // SRA (HL)
            // 2  16
            // Z 0 0 0
            0x2E => unreachable,
            // SRA A
            // 2  8
            // Z 0 0 0
            0x2F => unreachable,
            // SWAP B
            // 2  8
            // Z 0 0 0
            0x30 => unreachable,
            // SWAP C
            // 2  8
            // Z 0 0 0
            0x31 => unreachable,
            // SWAP D
            // 2  8
            // Z 0 0 0
            0x32 => unreachable,
            // SWAP E
            // 2  8
            // Z 0 0 0
            0x33 => unreachable,
            // SWAP H
            // 2  8
            // Z 0 0 0
            0x34 => unreachable,
            // SWAP L
            // 2  8
            // Z 0 0 0
            0x35 => unreachable,
            // SWAP (HL)
            // 2  16
            // Z 0 0 0
            0x36 => unreachable,
            // SWAP A
            // 2  8
            // Z 0 0 0
            0x37 => unreachable,
            // SRL B
            // 2  8
            // Z 0 0 C
            0x38 => unreachable,
            // SRL C
            // 2  8
            // Z 0 0 C
            0x39 => unreachable,
            // SRL D
            // 2  8
            // Z 0 0 C
            0x3A => unreachable,
            // SRL E
            // 2  8
            // Z 0 0 C
            0x3B => unreachable,
            // SRL H
            // 2  8
            // Z 0 0 C
            0x3C => unreachable,
            // SRL L
            // 2  8
            // Z 0 0 C
            0x3D => unreachable,
            // SRL (HL)
            // 2  16
            // Z 0 0 C
            0x3E => unreachable,
            // SRL A
            // 2  8
            // Z 0 0 C
            0x3F => unreachable,
            // BIT 0,B
            // 2  8
            // Z 0 1 -
            0x40 => unreachable,
            // BIT 0,C
            // 2  8
            // Z 0 1 -
            0x41 => unreachable,
            // BIT 0,D
            // 2  8
            // Z 0 1 -
            0x42 => unreachable,
            // BIT 0,E
            // 2  8
            // Z 0 1 -
            0x43 => unreachable,
            // BIT 0,H
            // 2  8
            // Z 0 1 -
            0x44 => unreachable,
            // BIT 0,L
            // 2  8
            // Z 0 1 -
            0x45 => unreachable,
            // BIT 0,(HL)
            // 2  16
            // Z 0 1 -
            0x46 => unreachable,
            // BIT 0,A
            // 2  8
            // Z 0 1 -
            0x47 => unreachable,
            // BIT 1,B
            // 2  8
            // Z 0 1 -
            0x48 => unreachable,
            // BIT 1,C
            // 2  8
            // Z 0 1 -
            0x49 => unreachable,
            // BIT 1,D
            // 2  8
            // Z 0 1 -
            0x4A => unreachable,
            // BIT 1,E
            // 2  8
            // Z 0 1 -
            0x4B => unreachable,
            // BIT 1,H
            // 2  8
            // Z 0 1 -
            0x4C => unreachable,
            // BIT 1,L
            // 2  8
            // Z 0 1 -
            0x4D => unreachable,
            // BIT 1,(HL)
            // 2  16
            // Z 0 1 -
            0x4E => unreachable,
            // BIT 1,A
            // 2  8
            // Z 0 1 -
            0x4F => unreachable,
            // BIT 2,B
            // 2  8
            // Z 0 1 -
            0x50 => unreachable,
            // BIT 2,C
            // 2  8
            // Z 0 1 -
            0x51 => unreachable,
            // BIT 2,D
            // 2  8
            // Z 0 1 -
            0x52 => unreachable,
            // BIT 2,E
            // 2  8
            // Z 0 1 -
            0x53 => unreachable,
            // BIT 2,H
            // 2  8
            // Z 0 1 -
            0x54 => unreachable,
            // BIT 2,L
            // 2  8
            // Z 0 1 -
            0x55 => unreachable,
            // BIT 2,(HL)
            // 2  16
            // Z 0 1 -
            0x56 => unreachable,
            // BIT 2,A
            // 2  8
            // Z 0 1 -
            0x57 => unreachable,
            // BIT 3,B
            // 2  8
            // Z 0 1 -
            0x58 => unreachable,
            // BIT 3,C
            // 2  8
            // Z 0 1 -
            0x59 => unreachable,
            // BIT 3,D
            // 2  8
            // Z 0 1 -
            0x5A => unreachable,
            // BIT 3,E
            // 2  8
            // Z 0 1 -
            0x5B => unreachable,
            // BIT 3,H
            // 2  8
            // Z 0 1 -
            0x5C => unreachable,
            // BIT 3,L
            // 2  8
            // Z 0 1 -
            0x5D => unreachable,
            // BIT 3,(HL)
            // 2  16
            // Z 0 1 -
            0x5E => unreachable,
            // BIT 3,A
            // 2  8
            // Z 0 1 -
            0x5F => unreachable,
            // BIT 4,B
            // 2  8
            // Z 0 1 -
            0x60 => unreachable,
            // BIT 4,C
            // 2  8
            // Z 0 1 -
            0x61 => unreachable,
            // BIT 4,D
            // 2  8
            // Z 0 1 -
            0x62 => unreachable,
            // BIT 4,E
            // 2  8
            // Z 0 1 -
            0x63 => unreachable,
            // BIT 4,H
            // 2  8
            // Z 0 1 -
            0x64 => unreachable,
            // BIT 4,L
            // 2  8
            // Z 0 1 -
            0x65 => unreachable,
            // BIT 4,(HL)
            // 2  16
            // Z 0 1 -
            0x66 => unreachable,
            // BIT 4,A
            // 2  8
            // Z 0 1 -
            0x67 => unreachable,
            // BIT 5,B
            // 2  8
            // Z 0 1 -
            0x68 => unreachable,
            // BIT 5,C
            // 2  8
            // Z 0 1 -
            0x69 => unreachable,
            // BIT 5,D
            // 2  8
            // Z 0 1 -
            0x6A => unreachable,
            // BIT 5,E
            // 2  8
            // Z 0 1 -
            0x6B => unreachable,
            // BIT 5,H
            // 2  8
            // Z 0 1 -
            0x6C => unreachable,
            // BIT 5,L
            // 2  8
            // Z 0 1 -
            0x6D => unreachable,
            // BIT 5,(HL)
            // 2  16
            // Z 0 1 -
            0x6E => unreachable,
            // BIT 5,A
            // 2  8
            // Z 0 1 -
            0x6F => unreachable,
            // BIT 6,B
            // 2  8
            // Z 0 1 -
            0x70 => unreachable,
            // BIT 6,C
            // 2  8
            // Z 0 1 -
            0x71 => unreachable,
            // BIT 6,D
            // 2  8
            // Z 0 1 -
            0x72 => unreachable,
            // BIT 6,E
            // 2  8
            // Z 0 1 -
            0x73 => unreachable,
            // BIT 6,H
            // 2  8
            // Z 0 1 -
            0x74 => unreachable,
            // BIT 6,L
            // 2  8
            // Z 0 1 -
            0x75 => unreachable,
            // BIT 6,(HL)
            // 2  16
            // Z 0 1 -
            0x76 => unreachable,
            // BIT 6,A
            // 2  8
            // Z 0 1 -
            0x77 => unreachable,
            // BIT 7,B
            // 2  8
            // Z 0 1 -
            0x78 => unreachable,
            // BIT 7,C
            // 2  8
            // Z 0 1 -
            0x79 => unreachable,
            // BIT 7,D
            // 2  8
            // Z 0 1 -
            0x7A => unreachable,
            // BIT 7,E
            // 2  8
            // Z 0 1 -
            0x7B => unreachable,
            // BIT 7,H
            // 2  8
            // Z 0 1 -
            0x7C => unreachable,
            // BIT 7,L
            // 2  8
            // Z 0 1 -
            0x7D => unreachable,
            // BIT 7,(HL)
            // 2  16
            // Z 0 1 -
            0x7E => unreachable,
            // BIT 7,A
            // 2  8
            // Z 0 1 -
            0x7F => unreachable,
            // RES 0,B
            // 2  8
            // - - - -
            0x80 => unreachable,
            // RES 0,C
            // 2  8
            // - - - -
            0x81 => unreachable,
            // RES 0,D
            // 2  8
            // - - - -
            0x82 => unreachable,
            // RES 0,E
            // 2  8
            // - - - -
            0x83 => unreachable,
            // RES 0,H
            // 2  8
            // - - - -
            0x84 => unreachable,
            // RES 0,L
            // 2  8
            // - - - -
            0x85 => unreachable,
            // RES 0,(HL)
            // 2  16
            // - - - -
            0x86 => unreachable,
            // RES 0,A
            // 2  8
            // - - - -
            0x87 => unreachable,
            // RES 1,B
            // 2  8
            // - - - -
            0x88 => unreachable,
            // RES 1,C
            // 2  8
            // - - - -
            0x89 => unreachable,
            // RES 1,D
            // 2  8
            // - - - -
            0x8A => unreachable,
            // RES 1,E
            // 2  8
            // - - - -
            0x8B => unreachable,
            // RES 1,H
            // 2  8
            // - - - -
            0x8C => unreachable,
            // RES 1,L
            // 2  8
            // - - - -
            0x8D => unreachable,
            // RES 1,(HL)
            // 2  16
            // - - - -
            0x8E => unreachable,
            // RES 1,A
            // 2  8
            //- - - -
            0x8F => unreachable,
            // RES 2,B
            // 2  8
            // - - - -
            0x90 => unreachable,
            // RES 2,C
            // 2  8
            // - - - -
            0x91 => unreachable,
            // RES 2,D
            // 2  8
            // - - - -
            0x92 => unreachable,
            // RES 2,E
            // 2  8
            // - - - -
            0x93 => unreachable,
            // RES 2,H
            // 2  8
            // - - - -
            0x94 => unreachable,
            // RES 2,L
            // 2  8
            // - - - -
            0x95 => unreachable,
            // RES 2,(HL)
            // 2  16
            // - - - -
            0x96 => unreachable,
            // RES 2,A
            // 2  8
            // - - - -
            0x97 => unreachable,
            // RES 3,B
            // 2  8
            // - - - -
            0x98 => unreachable,
            // RES 3,C
            // 2  8
            // - - - -
            0x99 => unreachable,
            // RES 3,D
            // 2  8
            // - - - -
            0x9A => unreachable,
            // RES 3,E
            // 2  8
            // - - - -
            0x9B => unreachable,
            // RES 3,H
            // 2  8
            // - - - -
            0x9C => unreachable,
            // RES 3,L
            // 2  8
            // - - - -
            0x9D => unreachable,
            // RES 3,(HL)
            // 2  16
            // - - - -
            0x9E => unreachable,
            // RES 3,A
            // 2  8
            // - - - -
            0x9F => unreachable,
            // RES 4,B
            // 2  8
            // - - - -
            0xA0 => unreachable,
            // RES 4,C
            // 2  8
            // - - - -
            0xA1 => unreachable,
            // RES 4,D
            // 2  8
            // - - - -
            0xA2 => unreachable,
            // RES 4,E
            // 2  8
            // - - - -
            0xA3 => unreachable,
            // RES 4,H
            // 2  8
            // - - - -
            0xA4 => unreachable,
            // RES 4,L
            // 2  8
            // - - - -
            0xA5 => unreachable,
            // RES 4,(HL)
            // 2  16
            // - - - -
            0xA6 => unreachable,
            // RES 4,A
            // 2  8
            // - - - -
            0xA7 => unreachable,
            // RES 5,B
            // 2  8
            // - - - -
            0xA8 => unreachable,
            // RES 5,C
            // 2  8
            // - - - -
            0xA9 => unreachable,
            // RES 5,D
            // 2  8
            // - - - -
            0xAA => unreachable,
            // RES 5,E
            // 2  8
            // - - - -
            0xAB => unreachable,
            // RES 5,H
            // 2  8
            // - - - -
            0xAC => unreachable,
            // RES 5,L
            // 2  8
            // - - - -
            0xAD => unreachable,
            // RES 5,(HL)
            // 2  16
            // - - - -
            0xAE => unreachable,
            // RES 5,A
            // 2  8
            // - - - -
            0xAF => unreachable,
            // RES 6,B
            // 2  8
            // - - - -
            0xB0 => unreachable,
            // RES 6,C
            // 2  8
            // - - - -
            0xB1 => unreachable,
            // RES 6,D
            // 2  8
            // - - - -
            0xB2 => unreachable,
            // RES 6,E
            // 2  8
            // - - - -
            0xB3 => unreachable,
            // RES 6,H
            // 2  8
            // - - - -
            0xB4 => unreachable,
            // RES 6,L
            // 2  8
            // - - - -
            0xB5 => unreachable,
            // RES 6,(HL)
            // 2  16
            // - - - -
            0xB6 => unreachable,
            // RES 6,A
            // 2  8
            // - - - -
            0xB7 => unreachable,
            // RES 7,B
            // 2  8
            // - - - -
            0xB8 => unreachable,
            // RES 7,C
            // 2  8
            // - - - -
            0xB9 => unreachable,
            // RES 7,D
            // 2  8
            // - - - -
            0xBA => unreachable,
            // RES 7,E
            // 2  8
            // - - - -
            0xBB => unreachable,
            // RES 7,H
            // 2  8
            // - - - -
            0xBC => unreachable,
            // RES 7,L
            // 2  8
            // - - - -
            0xBD => unreachable,
            // RES 7,(HL)
            // 2  16
            // - - - -
            0xBE => unreachable,
            // RES 7,A
            // 2  8
            // - - - -
            0xBF => unreachable,
            // SET 0,B
            // 2  8
            // - - - -
            0xC0 => unreachable,
            // SET 0,C
            // 2  8
            // - - - -
            0xC1 => unreachable,
            // SET 0,D
            // 2  8
            // - - - -
            0xC2 => unreachable,
            // SET 0,E
            // 2  8
            // - - - -
            0xC3 => unreachable,
            // SET 0,H
            // 2  8
            // - - - -
            0xC4 => unreachable,
            // SET 0,L
            // 2  8
            // - - - -
            0xC5 => unreachable,
            // SET 0,(HL)
            // 2  16
            // - - - -
            0xC6 => unreachable,
            // SET 0,A
            // 2  8
            // - - - -
            0xC7 => unreachable,
            // SET 1,B
            // 2  8
            // - - - -
            0xC8 => unreachable,
            // SET 1,C
            // 2  8
            // - - - -
            0xC9 => unreachable,
            // SET 1,D
            // 2  8
            // - - - -
            0xCA => unreachable,
            // SET 1,E
            // 2  8
            // - - - -
            0xCB => unreachable,
            // SET 1,H
            // 2  8
            // - - - -
            0xCC => unreachable,
            // SET 1,L
            // 2  8
            // - - - -
            0xCD => unreachable,
            // SET 1,(HL)
            // 2  16
            // - - - -
            0xCE => unreachable,
            // SET 1,A
            // 2  8
            // - - - -
            0xCF => unreachable,
            // SET 2,B
            // 2  8
            // - - - -
            0xD0 => unreachable,
            // SET 2,C
            // 2  8
            // - - - -
            0xD1 => unreachable,
            // SET 2,D
            // 2  8
            // - - - -
            0xD2 => unreachable,
            // SET 2,E
            // 2  8
            // - - - -
            0xD3 => unreachable,
            // SET 2,H
            // 2  8
            //- - - -
            0xD4 => unreachable,
            // SET 2,L
            // 2  8
            // - - - -
            0xD5 => unreachable,
            // SET 2,(HL)
            // 2  16
            // - - - -
            0xD6 => unreachable,
            // SET 2,A
            // 2  8
            // - - - -
            0xD7 => unreachable,
            // SET 3,B
            // 2  8
            // - - - -
            0xD8 => unreachable,
            // SET 3,C
            // 2  8
            // - - - -
            0xD9 => unreachable,
            // SET 3,D
            // 2  8
            // - - - -
            0xDA => unreachable,
            // SET 3,E
            // 2  8
            // - - - -
            0xDB => unreachable,
            // SET 3,H
            // 2  8
            // - - - -
            0xDC => unreachable,
            // SET 3,L
            // 2  8
            // - - - -
            0xDD => unreachable,
            // SET 3,(HL)
            // 2  16
            // - - - -
            0xDE => unreachable,
            // SET 3,A
            // 2  8
            // - - - -
            0xDF => unreachable,
            // SET 4,B
            // 2  8
            //- - - -
            0xE0 => unreachable,
            // SET 4,C
            // 2  8
            // - - - -
            0xE1 => unreachable,
            // SET 4,D
            // 2  8
            // - - - -
            0xE2 => unreachable,
            // SET 4,E
            // 2  8
            // - - - -
            0xE3 => unreachable,
            // SET 4,H
            // 2  8
            // - - - -
            0xE4 => unreachable,
            // SET 4,L
            // 2  8
            // - - - -
            0xE5 => unreachable,
            // SET 4,(HL)
            // 2  16
            // - - - -
            0xE6 => unreachable,
            // SET 4,A
            // 2  8
            // - - - -
            0xE7 => unreachable,
            // SET 5,B
            // 2  8
            // - - - -
            0xE8 => unreachable,
            // SET 5,C
            // 2  8
            // - - - -
            0xE9 => unreachable,
            // SET 5,D
            // 2  8
            // - - - -
            0xEA => unreachable,
            // SET 5,E
            // 2  8
            // - - - -
            0xEB => unreachable,
            // SET 5,H
            // 2  8
            // - - - -
            0xEC => unreachable,
            // SET 5,L
            // 2  8
            // - - - -
            0xED => unreachable,
            // SET 5,(HL)
            // 2  16
            // - - - -
            0xEE => unreachable,
            // SET 5,A
            // 2  8
            // - - - -
            0xEF => unreachable,
            // SET 6,B
            // 2  8
            // - - - -
            0xF0 => unreachable,
            // SET 6,C
            // 2  8
            //- - - -
            0xF1 => unreachable,
            // SET 6,D
            // 2  8
            // - - - -
            0xF2 => unreachable,
            // SET 6,E
            // 2  8
            // - - - -
            0xF3 => unreachable,
            // SET 6,H
            // 2  8
            // - - - -
            0xF4 => unreachable,
            // SET 6,L
            //2  8
            // - - - -
            0xF5 => unreachable,
            // SET 6,(HL)
            // 2  16
            // - - - -
            0xF6 => unreachable,
            // SET 6,A
            // 2  8
            // - - - -
            0xF7 => unreachable,
            // SET 7,B
            // 2  8
            // - - - -
            0xF8 => unreachable,
            // SET 7,C
            // 2  8
            // - - - -
            0xF9 => unreachable,
            // SET 7,D
            // 2  8
            // - - - -
            0xFA => unreachable,
            // SET 7,E
            // 2  8
            // - - - -
            0xFB => unreachable,
            // SET 7,H
            // 2  8
            // - - - -
            0xFC => unreachable,
            // SET 7,L
            // 2  8
            // - - - -
            0xFD => unreachable,
            // SET 7,(HL)
            // 2  16
            // - - - -
            0xFE => unreachable,
            // SET 7,A
            // 2  8
            // - - - -
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

    std.debug.print("This instruction took ${x} cycles\n", .{cycles});
}
