const std = @import("std");

pub const Mapper = enum { ROM_ONLY, MBC1, MBC2, MBC3, MBC5, MBC6, MBC7, MMM01, HuC1, HuC3 };

pub const CartridgeHeader = struct {
    entry_point: []u8,
    nintendo_logo: []u8,
    title: []u8,
    manufacturer_code: []u8,
    cgb_flag: u8,
    new_licensee_code: []u8,
    sgb_flag: u8,
    cartridge_type: u8,
    rom_size: u8,
    ram_size: u8,
    destination_code: u8,
    old_licensee_code: u8,
    mask_rom_version_number: u8,
    header_checksum: u8,
    global_checksum: []u8,

    pub fn init(rom_data: []u8) CartridgeHeader {
        return CartridgeHeader{
            .entry_point = rom_data[0x100..0x104],
            .nintendo_logo = rom_data[0x104..0x134],
            .title = rom_data[0x134..0x144],
            .manufacturer_code = rom_data[0x13F..0x143],
            .cgb_flag = rom_data[0x143],
            .new_licensee_code = rom_data[0x144..0x146],
            .sgb_flag = rom_data[0x146],
            .cartridge_type = rom_data[0x147],
            .rom_size = rom_data[0x148],
            .ram_size = rom_data[0x149],
            .destination_code = rom_data[0x14A],
            .old_licensee_code = rom_data[0x14B],
            .mask_rom_version_number = rom_data[0x14C],
            .header_checksum = rom_data[0x14D],
            .global_checksum = rom_data[0x14E..0x150],
        };
    }

    pub fn getMapper(self: CartridgeHeader) Mapper {
        return switch (self.cartridge_type) {
            0x01 => Mapper.MBC1,
            0x02 => Mapper.MBC1,
            0x03 => Mapper.MBC1,
            0x05 => Mapper.MBC2,
            0x06 => Mapper.MBC2,
            0x08 => Mapper.ROM_ONLY,
            0x09 => Mapper.ROM_ONLY,
            0x0B => Mapper.MMM01,
            0x0C => Mapper.MMM01,
            0x0D => Mapper.MMM01,
            0x0F => Mapper.MBC3,
            0x10 => Mapper.MBC3,
            0x11 => Mapper.MBC3,
            0x12 => Mapper.MBC3,
            0x13 => Mapper.MBC3,
            0x19 => Mapper.MBC5,
            0x1A => Mapper.MBC5,
            0x1B => Mapper.MBC5,
            0x1C => Mapper.MBC5,
            0x1D => Mapper.MBC5,
            0x1E => Mapper.MBC5,
            0x20 => Mapper.MBC6,
            0x22 => Mapper.MBC7,
            0xFE => Mapper.HuC3,
            0xFF => Mapper.HuC1,
            else => unreachable,
        };
    }

    pub fn pp(self: CartridgeHeader, stdout: anytype) !void {
        try stdout.print("Catridge Header for {s}:\n", .{self.title});
        try stdout.print("\tMapper:", .{});
        switch (self.getMapper()) {
            Mapper.ROM_ONLY => try stdout.print("ROM ONLY\n", .{}),
            Mapper.MBC1 => try stdout.print("MBC1\n", .{}),
            Mapper.MBC2 => try stdout.print("MBC2\n", .{}),
            Mapper.MBC3 => try stdout.print("MBC3\n", .{}),
            Mapper.MBC5 => try stdout.print("MBC5\n", .{}),
            Mapper.MBC7 => try stdout.print("MBC7\n", .{}),
            Mapper.MMM01 => try stdout.print("MMM01\n", .{}),
            Mapper.HuC1 => try stdout.print("HuC1\n", .{}),
            Mapper.HuC3 => try stdout.print("HuC3\n", .{}),
            else => unreachable,
        }
    }
};
