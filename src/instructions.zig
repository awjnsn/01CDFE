const std = @import("std");
const st = @import("state.zig");
const regs = st.Regs;
const flags = st.Flags;
const cc = st.Cond;

pub fn halfCarry(a: u16, b: u16) bool {
    // Who's to say if this is right?
    return (((a & 0xF) + (b & 0xF)) & 0x10) == 0x10;
}

pub const Instruction = struct {
    state: *st.State,

    pub fn init(state: *st.State) Instruction {
        return Instruction{ .state = state };
    }

    pub fn jp(self: *const Instruction, address: u16) u8 {
        self.state.resetFlags();
        std.debug.print("JP ${X}\n", .{address});
        self.state.setReg(regs.PC, address);
        return 16;
    }

    pub fn nop(self: *const Instruction) u8 {
        self.state.resetFlags();
        std.debug.print("NOP\n", .{});
        self.state.incPC();
        return 4;
    }

    pub fn xor(self: *const Instruction, reg: regs, deref: bool) u8 {
        self.state.resetFlags();
        std.debug.print("XOR ", .{});

        var x: u16 = undefined;

        if (deref) {
            std.debug.print("(", .{});
            st.printReg(reg);
            std.debug.print(")", .{});
            x = self.state.readUnsignedByte(self.state.getReg(reg));
        } else {
            st.printReg(reg);
            x = self.state.getReg(reg);
        }

        std.debug.print("\n", .{});

        const res = self.state.getReg(regs.A) ^ x;

        self.state.setReg(regs.A, res);
        self.state.setFlag(flags.Z, res == 0);
        self.state.incPC();
        return 4;
    }

    pub fn ldImm16(self: *const Instruction, reg: regs, imm: u16) u8 {
        self.state.resetFlags();
        std.debug.print("LD ", .{});
        st.printReg(reg);
        std.debug.print(", ${X}\n", .{imm});
        self.state.setReg(reg, imm);
        self.state.setReg(regs.PC, self.state.getReg(regs.PC) + 3);
        return 12;
    }

    pub fn ldImm8(self: *const Instruction, reg: regs, imm: u8, deref: bool) u8 {
        self.state.resetFlags();
        std.debug.print("LD ", .{});
        if (deref) {
            std.debug.print("(", .{});
            st.printReg(reg);
            std.debug.print(")", .{});
            self.state.writeByte(self.state.getReg(reg), imm);
        } else {
            st.printReg(reg);
            self.state.setReg(reg, imm);
        }
        std.debug.print(" ${X}\n", .{imm});
        self.state.setReg(regs.PC, self.state.getReg(regs.PC) + 2);
        return if (deref) 12 else 8;
    }

    pub fn ldReg(self: *const Instruction, dst: regs, src: regs, derefDst: bool, derefSrc: bool, incDst: bool, decDst: bool) u8 {
        self.state.resetFlags();
        std.debug.print("LD ", .{});
        if (derefDst) {
            std.debug.print("(", .{});
            st.printReg(dst);
            if (incDst) {
                std.debug.print("+", .{});
            } else if (decDst) {
                std.debug.print("-", .{});
            }
            std.debug.print(")", .{});
        } else {
            st.printReg(dst);
        }
        std.debug.print(", ", .{});
        var srcVal: u8 = undefined;
        if (derefSrc) {
            std.debug.print("(", .{});
            st.printReg(src);
            std.debug.print(")", .{});
            srcVal = self.state.readUnsignedByte(self.state.getReg(src));
        } else {
            st.printReg(src);
            srcVal = @truncate(self.state.getReg(src));
        }

        std.debug.print("\n", .{});

        if (derefDst) {
            self.state.writeByte(self.state.getReg(dst), srcVal);
        } else {
            self.state.setReg(dst, @as(u16, srcVal));
        }

        if (incDst) {
            self.state.setReg(dst, self.state.getReg(dst) + 1);
        } else if (decDst) {
            self.state.setReg(dst, self.state.getReg(dst) - 1);
        }

        self.state.incPC();
        return if (derefDst or derefSrc) 8 else 4;
    }

    pub fn dec(self: *const Instruction, reg: regs, deref: bool) u8 {
        self.state.resetFlags();
        std.debug.print("DEC ", .{});
        var val: u16 = undefined;
        if (deref) {
            std.debug.print("(", .{});
            st.printReg(reg);
            std.debug.print(")", .{});
            val = self.state.readUnsignedByte(self.state.getReg(reg));
        } else {
            st.printReg(reg);
            val = self.state.getReg(reg);
        }

        std.debug.print("\n", .{});

        var res: u16 = undefined;
        var underflow: bool = false;
        if (st.State.isSingleByteReg(reg) or deref) {
            if (val == 0x0) {
                underflow = true;
                res = 0xFF;
            } else {
                res = val - 1;
            }
        } else {
            if (val == 0x0) {
                underflow = true;
                res = 0xFFFF;
            } else {
                res = val - 1;
            }
        }

        if (st.State.isSingleByteReg(reg) or deref) {
            if (res == 0) {
                self.state.setFlag(flags.Z, true);
            }
            self.state.setFlag(flags.N, true);
            if (halfCarry(res, 1)) {
                self.state.setFlag(flags.H, true);
            }
        }

        if (deref) {
            self.state.writeByte(self.state.getReg(reg), @truncate(res));
        } else {
            self.state.setReg(reg, res);
        }

        self.state.incPC();
        if (deref) {
            return 12;
        } else if (st.State.isSingleByteReg(reg)) {
            return 4;
        } else {
            return 8;
        }
    }

    pub fn inc(self: *const Instruction, reg: regs, deref: bool) u8 {
        self.state.resetFlags();
        std.debug.print("INC ", .{});
        var val: u16 = undefined;
        if (deref) {
            std.debug.print("(", .{});
            st.printReg(reg);
            std.debug.print(")", .{});
            val = self.state.readUnsignedByte(self.state.getReg(reg));
        } else {
            st.printReg(reg);
            val = self.state.getReg(reg);
        }

        std.debug.print("\n", .{});

        var res: u16 = undefined;
        var overflow: bool = false;
        if (st.State.isSingleByteReg(reg) or deref) {
            if (val == 0xFF) {
                overflow = true;
                res = 0;
            } else {
                res = val + 1;
            }
        } else {
            if (val == 0xFFFF) {
                overflow = true;
                res = 0;
            } else {
                res = val + 1;
            }
        }

        if (st.State.isSingleByteReg(reg) or deref) {
            if (res == 0) {
                self.state.setFlag(flags.Z, true);
            }

            if (halfCarry(res, 1)) {
                self.state.setFlag(flags.H, true);
            }
        }

        if (deref) {
            self.state.writeByte(self.state.getReg(reg), @truncate(res));
        } else {
            self.state.setReg(reg, res);
        }

        self.state.incPC();
        if (deref) {
            return 12;
        } else if (st.State.isSingleByteReg(reg)) {
            return 4;
        } else {
            return 8;
        }
    }

    pub fn jr(self: *const Instruction, offset: i8) u8 {
        self.state.resetFlags();
        std.debug.print("JR {x}", .{offset});
        const negative: bool = offset < 0;
        const absOffset: i16 = @as(i16, if (negative) (-1 * offset) else (offset));
        const absOffsetUnsigned: u16 = @bitCast(absOffset);
        const address: u16 = self.state.getReg(regs.PC);

        if (negative) {
            self.state.setReg(regs.PC, address + 1 - absOffsetUnsigned);
        } else {
            self.state.setReg(regs.PC, address + 1 + absOffsetUnsigned);
        }
        return 12;
    }

    pub fn jrCond(self: *const Instruction, cond: cc, offset: i8) u8 {
        std.debug.print("JR ", .{});
        st.printCC(cond);
        std.debug.print(", ${x}\n", .{offset});

        const condMet: bool = self.state.getCC(cond);
        self.state.resetFlags();

        if (!condMet) {
            self.state.setReg(regs.PC, self.state.getReg(regs.PC) + 2);
            return 8;
        }

        const negative: bool = offset < 0;
        const absOffset: i16 = @as(i16, if (negative) (-1 * offset) else (offset));
        const absOffsetUnsigned: u16 = @bitCast(absOffset);
        const address: u16 = self.state.getReg(regs.PC);

        if (negative) {
            self.state.setReg(regs.PC, address + 2 - absOffsetUnsigned);
        } else {
            self.state.setReg(regs.PC, address + 2 + absOffsetUnsigned);
        }
        return 12;
    }

    pub fn ime(self: *const Instruction, flag: bool) u8 {
        self.state.resetFlags();

        if (flag) {
            std.debug.print("EI\n", .{});
        } else {
            std.debug.print("DI\n", .{});
        }

        self.state.setFlag(flags.IME, flag);

        self.state.incPC();

        return 4;
    }

    pub fn ldhImm8A(self: *const Instruction, a8: u8) u8 {
        self.state.resetFlags();
        std.debug.print("LDH (${X}),A\n", .{a8});
        const dest: u16 = @as(u16, a8) + 0xFF00;
        self.state.writeByte(dest, @truncate(self.state.getReg(regs.A)));
        self.state.setReg(regs.PC, self.state.getReg(regs.PC) + 2);
        return 12;
    }

    pub fn ldhAImm8(self: *const Instruction, a8: u8) u8 {
        self.state.resetFlags();
        std.debug.print("LDH A,(${X})\n", .{a8});
        const src: u16 = @as(u16, a8) + 0xFF00;
        self.state.setReg(regs.A, self.state.readUnsignedByte(src));
        self.state.setReg(regs.PC, self.state.getReg(regs.PC) + 2);
        return 12;
    }

    pub fn ldImm16A(self: *const Instruction, dest: u16) u8 {
        self.state.resetFlags();
        std.debug.print("LD (${X}),A\n", .{dest});
        self.state.writeByte(dest, @truncate(self.state.getReg(regs.A)));
        self.state.setReg(regs.PC, self.state.getReg(regs.PC) + 3);
        return 16;
    }

    pub fn ldAImm16(self: *const Instruction, src: u16) u8 {
        self.state.resetFlags();
        self.state.setReg(regs.A, self.state.readUnsignedByte(src));
        std.debug.print("LD A,(${X})\n", .{src});
        self.state.setReg(regs.PC, self.state.getReg(regs.PC) + 3);
        return 16;
    }

    pub fn _cp(self: *const Instruction, val: u8) void {
        const a: u8 = @truncate(self.state.getReg(regs.A));
        self.state.setFlag(flags.Z, a == val);
        self.state.setFlag(flags.N, true);
        self.state.setFlag(flags.H, halfCarry(a, val));
        self.state.setFlag(flags.C, val > a);
    }

    pub fn cpReg(self: *const Instruction, reg: regs, deref: bool) u8 {
        self.state.resetFlags();
        std.debug.print("CP ", .{});
        if (deref) {
            std.debug.print("(", .{});
            st.printReg(reg);
            std.debug.print(")", .{});
            self._cp(self.state.readUnsignedByte(self.state.getReg(reg)));
        } else {
            st.printReg(reg);
            self._cp(@truncate(self.state.getReg(reg)));
        }
        std.debug.print("\n", .{});
        self.state.incPC();
        return if (deref) 8 else 4;
    }

    pub fn cpImm8(self: *const Instruction, imm: u8) u8 {
        self.state.resetFlags();
        std.debug.print("CP ${X}\n", .{imm});
        self._cp(imm);
        self.state.setReg(regs.PC, self.state.getReg(regs.PC) + 2);
        return 8;
    }
};
