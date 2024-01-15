const std = @import("std");
const st = @import("state.zig");
const regs = st.Regs;
const flags = st.Flags;

pub const Instruction = struct {
    state: *st.State,

    pub fn init(state: *st.State) Instruction {
        return Instruction {
            .state = state
        };
    }

    pub fn nop(self: *const Instruction) u8 {
        std.debug.print("NOP\n", .{});
        self.state.setReg(regs.PC, self.state.getReg(regs.PC) + 1);
        return 4;
    }

    pub fn jp(self: *const Instruction, address: u16) u8 {
        self.state.setReg(regs.PC, address);
        return 16;
    }
};
