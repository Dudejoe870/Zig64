const std = @import("std");
const memory = @import("memory.zig");
const R4300 = @import("R4300.zig");

pub fn init(bootrom_path: ?[]const u8, rom_path: []const u8) !void {
    memory.init();

    var rom_file = try std.fs.cwd().openFile(rom_path, .{ });
    defer rom_file.close();
    _ = try memory.cart.rom.writeFromFile(rom_file);

    if (bootrom_path) |path| {
        var bootrom_file = try std.fs.cwd().openFile(path, .{ });
        defer bootrom_file.close();
        _ = try memory.pif.boot_rom.writeFromFile(bootrom_file);
    }
    
    R4300.init(bootrom_path == null);
}

pub fn step() !void {
    try R4300.step();
}
