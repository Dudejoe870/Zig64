const std = @import("std");
const memory = @import("memory.zig");
const R4300 = @import("R4300.zig");

pub fn init(bootrom_path: ?[]const u8, rom_path: []const u8) void {
    _ = rom_path; // TODO

    memory.init();
    R4300.init(bootrom_path == null);
}
