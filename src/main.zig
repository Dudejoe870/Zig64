const std = @import("std");

const System = @import("System.zig");

pub fn main() !u8 {
    var arena = std.heap.ArenaAllocator.init(std.heap.raw_c_allocator);
    defer arena.deinit();

    var arg_it = try std.process.argsWithAllocator(arena.allocator());
    _ = arg_it.skip();

    const rom = arg_it.next() orelse {
        std.log.err("Expected first argument to be rom to run", .{ });
        return error.InvalidArgs;
    };
    const pif = arg_it.next() orelse {
        std.log.err("Expected first argument to be PIF rom.", .{ });
        return error.InvalidArgs;
    };

    try runEmulator(pif, rom);
    return 0;
}

fn runEmulator(bootrom_path: ?[]const u8, rom_path: []const u8) !void {
    try System.init(bootrom_path, rom_path);

    while (true) {
        try System.step();
    }
}
