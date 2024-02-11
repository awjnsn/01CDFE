const std = @import("std");
const st = @import("state.zig");
const regs = st.Regs;
const flags = st.Flags;

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
        self.state.setReg(regs.PC, self.state.getReg(regs.PC) + 1);
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
            x = self.state.memory[self.state.getReg(reg)];
        } else {
            st.printReg(reg);
            x = self.state.getReg(reg);
        }

        std.debug.print("\n", .{});

        const res = self.state.getReg(regs.A) ^ x;

        self.state.setReg(regs.A, res);
        self.state.setFlag(flags.Z, res == 0);
        self.state.setReg(regs.PC, self.state.getReg(regs.PC) + 1);
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
            self.state.memory[self.state.getReg(reg)] = imm;
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
            if(incDst) {
                std.debug.print("+", .{});
            } else if(decDst) {
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
            srcVal = self.state.memory[self.state.getReg(src)];
        }
        else {
            st.printReg(src);
            srcVal = @truncate(self.state.getReg(src));
        }

        std.debug.print("\n", .{});

        if (derefDst) {
            self.state.memory[self.state.getReg(dst)] = srcVal;
        } else {
            self.state.setReg(dst, @as(u16, srcVal));
        }

        if(incDst) {
            self.state.setReg(dst, self.state.getReg(dst) + 1);
        } else if (decDst) {
            self.state.setReg(dst, self.state.getReg(dst) - 1);
        }


        self.state.setReg(regs.PC, self.state.getReg(regs.PC) + 1);
        return if (derefDst or derefSrc) 8 else 4;
    }

};
