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
        self.state.setReg(regs.PC, address);
        return 16;
    }

    pub fn nop(self: *const Instruction) u8 {
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
};
