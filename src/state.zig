const std = @import("std");
const cartHeader = @import("cartHeader.zig");

pub const Regs = enum { AF, A, BC, B, C, DE, D, E, HL, H, L, SP, PC };
pub const Flags = enum { Z, N, H, C };

pub const State = struct {
    memory: [0x10000]u8,
    reg_AF: u16,
    reg_BC: u16,
    reg_DE: u16,
    reg_HL: u16,
    reg_SP: u16,
    reg_PC: u16,

    pub fn isSingleByteReg(reg: Regs) bool {
        return switch (reg) {
            Regs.A => true,
            Regs.B => true,
            Regs.C => true,
            Regs.D => true,
            Regs.E => true,
            Regs.H => true,
            Regs.L => true,
            else => false,
        };
    }

    pub fn setFlag(self: *State, flag: Flags, val: bool) void {
        switch (flag) {
            Flags.Z => self.reg_AF ^= if (val) (0x1 << 0x7) else 0x0,
            Flags.N => self.reg_AF ^= if (val) (0x1 << 0x6) else 0x0,
            Flags.H => self.reg_AF ^= if (val) (0x1 << 0x5) else 0x0,
            Flags.C => self.reg_AF ^= if (val) (0x1 << 0x4) else 0x0,
        }
    }

    pub fn getFlag(self: *State, flag: Flags) bool {
        return switch (flag) {
            Flags.Z => (self.reg_AF & (0x1 << 0x7)) > 0,
            Flags.N => (self.reg_AF & (0x1 << 0x6)) > 0,
            Flags.H => (self.reg_AF & (0x1 << 0x5)) > 0,
            Flags.C => (self.reg_AF & (0x1 << 0x4)) > 0,
        };
    }

    pub fn setReg(self: *State, reg: Regs, val: u16) void {
        // If the value is too large for the register
        if (isSingleByteReg(reg) and (val > 0xFF)) unreachable;

        switch (reg) {
            Regs.AF => unreachable, // F cannot be set directly
            Regs.A => self.reg_AF = (self.reg_AF & 0x00FF) + (val << 8),
            Regs.BC => self.reg_BC = val,
            Regs.B => self.reg_BC = (self.reg_BC & 0x00FF) + (val << 8),
            Regs.C => self.reg_BC = (self.reg_BC & 0xFF00) + val,
            Regs.DE => self.reg_DE = val,
            Regs.D => self.reg_DE = (self.reg_DE & 0x00FF) + (val << 8),
            Regs.E => self.reg_DE = (self.reg_DE & 0xFF00) + val,
            Regs.HL => self.reg_HL = val,
            Regs.H => self.reg_HL = (self.reg_HL & 0x00FF) + (val << 8),
            Regs.L => self.reg_HL = (self.reg_HL & 0xFF00) + val,
            Regs.SP => self.reg_SP = val,
            Regs.PC => self.reg_PC = val,
        }
    }

    pub fn getReg(self: *State, reg: Regs) u16 {
        return switch (reg) {
            Regs.AF => self.reg_AF,
            Regs.A => self.reg_AF >> 8,
            Regs.BC => self.reg_BC,
            Regs.B => self.reg_BC >> 8,
            Regs.C => self.reg_BC & 0x00FF,
            Regs.DE => self.reg_DE,
            Regs.D => self.reg_DE >> 8,
            Regs.E => self.reg_DE & 0x00FF,
            Regs.HL => self.reg_HL,
            Regs.H => self.reg_HL >> 8,
            Regs.L => self.reg_HL & 0x00FF,
            Regs.SP => self.reg_SP,
            Regs.PC => self.reg_PC,
        };
    }

    pub fn init() State {
        return State{
            .memory = [_]u8{0} ** 0x10000,
            .reg_AF = 0x0000,
            .reg_BC = 0,
            .reg_DE = 0,
            .reg_HL = 0,
            .reg_SP = 0,
            .reg_PC = 0x100,
        };
    }

    pub fn mapMemory(self: *State, rom_data: []u8, mapper: cartHeader.Mapper) void {
        switch (mapper) {
            cartHeader.Mapper.ROM_ONLY => std.mem.copyForwards(u8, &self.memory, rom_data),
            else => std.debug.print("Unsupported!\n", .{}),
        }
        std.debug.print("Mapping {d} bytes\n", .{rom_data.len});
    }

    pub fn pp(self: *State, stdout: anytype) !void {
        try stdout.print("Register State:\n", .{});
        try stdout.print("AF: 0x{x:0>4}\n", .{self.getReg(Regs.AF)});
        try stdout.print(" A: 0x{x:0>2}\n", .{self.getReg(Regs.A)});
        try stdout.print("BC: 0x{x:0>4}\n", .{self.getReg(Regs.BC)});
        try stdout.print(" B: 0x{x:0>2}\n", .{self.getReg(Regs.B)});
        try stdout.print(" C: 0x{x:0>2}\n", .{self.getReg(Regs.C)});
        try stdout.print("DE: 0x{x:0>4}\n", .{self.getReg(Regs.DE)});
        try stdout.print(" D: 0x{x:0>2}\n", .{self.getReg(Regs.D)});
        try stdout.print(" E: 0x{x:0>2}\n", .{self.getReg(Regs.E)});
        try stdout.print("HL: 0x{x:0>4}\n", .{self.getReg(Regs.HL)});
        try stdout.print(" H: 0x{x:0>2}\n", .{self.getReg(Regs.H)});
        try stdout.print(" L: 0x{x:0>2}\n", .{self.getReg(Regs.L)});
        try stdout.print("SP: 0x{x:0>4}\n", .{self.getReg(Regs.SP)});
        try stdout.print("PC: 0x{x:0>4}\n", .{self.getReg(Regs.PC)});
        try stdout.print("Flag State ", .{});
        try stdout.print("[Z: {}] ", .{self.getFlag(Flags.Z)});
        try stdout.print("[N: {}] ", .{self.getFlag(Flags.N)});
        try stdout.print("[H: {}] ", .{self.getFlag(Flags.H)});
        try stdout.print("[C: {}]\n\n", .{self.getFlag(Flags.C)});
    }

    pub fn dumpMem(self: *State) void {
        const wrap: u16 = 25; // Keep output lines constrained to 80 chars
        for (self.memory, 0..) |byte, addr| {
            if (addr % wrap == 0) {
                if (addr > 0) {
                    std.debug.print("\n", .{});
                }
                std.debug.print("{x:0>4} ", .{addr});
            }
            std.debug.print("{x:0>2} ", .{byte});
        }
        std.debug.print("\n", .{});
    }
};

test "State Change" {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();
    var testState: State = State.init();
    try stdout.print("Initial state\n\n", .{});
    try testState.pp(stdout);
    try stdout.print("Changing PC\n\n", .{});
    testState.setReg(Regs.PC, 0xBEEF);
    try std.testing.expectEqual(testState.getReg(Regs.PC), 0xBEEF);
    try testState.pp(stdout);
    try stdout.print("Setting Z flag\n\n", .{});
    testState.setFlag(Flags.Z, true);
    try std.testing.expectEqual(testState.getFlag(Flags.Z), true);
    try testState.pp(stdout);
    try bw.flush();
}
