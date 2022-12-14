const std = @import("std");
const memory = @import("memory.zig");
const r4300 = @import("r4300.zig");

pub var paused = false;
pub const SingleStepSetting = enum {
    r4300_cpu,
    rsp,
    rdp
};
var single_step_setting: SingleStepSetting = .r4300_cpu;

var should_single_step = false;

pub fn init(bootrom_path: ?[]const u8, rom_path: []const u8) !void {
    try memory.init();

    var rom_file = try std.fs.cwd().openFile(rom_path, .{ });
    defer rom_file.close();
    try memory.cart.init(rom_file);

    if (bootrom_path) |path| {
        var bootrom_file = try std.fs.cwd().openFile(path, .{ });
        defer bootrom_file.close();
        try memory.pif.init(bootrom_file);
    } else {
        try memory.pif.init(null);
    }
    
    r4300.init(bootrom_path == null);
}

pub inline fn setSingleStepSetting(value: SingleStepSetting) void {
    should_single_step = false;
    single_step_setting = value;
}

pub inline fn getSingleStepSetting() SingleStepSetting {
    return single_step_setting;
}

pub inline fn singleStep() void {
    should_single_step = true;
}

pub fn step() !void {
    if (paused and !should_single_step) {
        std.time.sleep(std.time.ns_per_ms * 100);
        return;
    }
    try r4300.step();
    if (single_step_setting == .r4300_cpu) {
        should_single_step = false;
    }
}
