const std = @import("std");

const memory = @import("memory.zig");
const system = @import("system.zig");

var clock: u32 = 0;
var refresh_rate: u32 = 0;

var last_current_increment_count: u64 = 0;
var current_field: u1 = 0;

var vi_count: u64 = 0;
var vi_timer: std.time.Timer = undefined;
pub var vis_per_second: u64 = 0;

pub fn init() !void {
    clock = switch(system.config.tv_type) {
        .ntsc => 48681812,
        .pal => 49656530,
        .mpal => 48628316
    };
    refresh_rate = switch(system.config.tv_type) {
        .ntsc => 60,
        .pal => 50,
        .mpal => 60
    };

    vi_timer = try std.time.Timer.start();
}

pub fn step() !void {
    const vi_v_sync = memory.rcp.vi.reg_range.getWordPtr(@enumToInt(memory.rcp.vi.RegRangeOffset.vi_v_sync_reg)).*;

    const count_per_vi_scanline = (clock / refresh_rate) / (vi_v_sync + 1);
    if ((system.r4300.inst_count - last_current_increment_count) >= count_per_vi_scanline) {
        const vi_current = memory.rcp.vi.reg_range.getWordPtr(@enumToInt(memory.rcp.vi.RegRangeOffset.vi_current_reg));
        const vi_intr = memory.rcp.vi.reg_range.getWordPtr(@enumToInt(memory.rcp.vi.RegRangeOffset.vi_intr_reg)).*;

        last_current_increment_count = system.r4300.inst_count;

        // Increment current VI scanline, ignoring the top bit.
        var current_counter = (((vi_current.* >> 1) + 1) << 1);
        if (current_counter >= vi_v_sync & ~@as(u32, 0b1)) {
            current_counter = 0;
        }

        // NTSC reserves the first 39 lines for v-blank. And thus, once we reach this point, V-Blank is done.
        // TODO: Check if this is how PAL and MPAL works. Write a test on hardware and verify VI_CURRENT behaviour.
        if (current_counter == 39) {
            current_field ^= 1;
        }

        vi_current.* = current_counter;
        if (vi_v_sync & 0b1 == 0) {
            vi_current.* |= current_field;
        }

        // TODO: Verify that two VI interrupts happen in interlaced modes on hardware?
        // Perhaps they only happen on the second field?
        if (current_counter == vi_intr & ~@as(u32, 0b1)) {
            // TODO: VI Interrupt
            vi_count += 1;
        }
    }

    if (vi_timer.read() >= std.time.ns_per_s) {
        vis_per_second = vi_count;
        vi_timer.reset();
        vi_count = 0;
    }
}
