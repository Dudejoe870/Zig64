const std = @import("std");
const Memory = @import("Memory.zig");

pub const rdram_base_addr: u32 = 0x00000000;
pub const rdram_reg_base_addr: u32 = 0x03f00000;

pub const pif_boot_rom_addr: u32 = 0x1fc00000;
