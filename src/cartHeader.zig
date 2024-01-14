const std = @import("std");

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
        return CartridgeHeader {
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

    pub fn pp(self: CartridgeHeader, stdout: anytype) !void {
        try stdout.print("Catridge Header for {s}:\n", .{self.title});
    }
};
