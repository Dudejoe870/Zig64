const std = @import("std");
const memory = @import("memory.zig");

pub const vi = @import("vi.zig");
pub const r4300 = @import("r4300.zig");
pub const rsp = @import("rsp.zig");

pub const TvType = enum(u8) {
    pal = 0,
    ntsc = 1,
    mpal = 2
};

pub const Config = struct {
    tv_type: TvType,
    hle_pif: bool
};

pub var paused = false;
pub const SingleStepSetting = enum {
    r4300_cpu,
    rsp,
    rdp
};
var single_step_setting: SingleStepSetting = .r4300_cpu;

var should_single_step = false;

pub var config: Config = undefined;

pub fn init(bootrom_path: ?[]const u8, rom_path: []const u8, conf: Config) !void {
    config = conf;
    config.hle_pif = bootrom_path == null;

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
    
    try vi.init();
    rsp.init();
    r4300.init();
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

// Runs at 31.25 MHz in the context of the CPU (93.75MHz) aka a 3rd of the speed of the CPU.
pub inline fn step() !void {
    // Step the other auxiliary systems.
    try vi.step(); // This doesn't need to run super often so 31.25 MHz is okay (it only updates infrequently)

    // Step the CPU and RSP.
    // The ratio between the CPU and RSP is 3:2, so we do a pattern of
    // cpu, rsp, cpu, rsp, cpu, (repeat) cpu, rsp, cpu, rsp, cpu, etc.
    try r4300.step();
    if (single_step_setting == .r4300_cpu) {
        while (paused and !should_single_step) { 
            std.time.sleep(std.time.ns_per_ms * 100);
        }
        should_single_step = false;
    }
    try rsp.step();
    if (single_step_setting == .rsp) {
        while (paused and !should_single_step) { 
            std.time.sleep(std.time.ns_per_ms * 100);
        }
        should_single_step = false;
    }
    try r4300.step();
    if (single_step_setting == .r4300_cpu) {
        while (paused and !should_single_step) { 
            std.time.sleep(std.time.ns_per_ms * 100);
        }
        should_single_step = false;
    }
    try rsp.step();
    if (single_step_setting == .rsp) {
        while (paused and !should_single_step) { 
            std.time.sleep(std.time.ns_per_ms * 100);
        }
        should_single_step = false;
    }
    try r4300.step();
    if (single_step_setting == .r4300_cpu) {
        while (paused and !should_single_step) { 
            std.time.sleep(std.time.ns_per_ms * 100);
        }
        should_single_step = false;
    }
}
