const std = @import("std");

const memory = @import("memory.zig");

const log = std.log.scoped(.cic);

pub const CicVersion = enum {
    _5101,
    X101,
    X102,
    X103,
    X105,
    X106,
    _5167,
    _8303,
    _8401,
    _8501
};

pub const CicInfo = struct {
    seed: u8,
    version: CicVersion
};

pub const cics = init: {
    var list = std.EnumArray(CicVersion, CicInfo).initUndefined();
    list.set(._5101, .{ .seed = 0xAC, .version = ._5101 });
    list.set(.X101, .{ .seed = 0x3F, .version = .X101 });
    list.set(.X102, .{ .seed = 0x3F, .version = .X102 });
    list.set(.X103, .{ .seed = 0x78, .version = .X103 });
    list.set(.X105, .{ .seed = 0x91, .version = .X105 });
    list.set(.X106, .{ .seed = 0x85, .version = .X106 });
    list.set(._5167, .{ .seed = 0xDD, .version = ._5167 });
    list.set(._8303, .{ .seed = 0xDD, .version = ._8303 });
    list.set(._8401, .{ .seed = 0xDD, .version = ._8401 });
    list.set(._8501, .{ .seed = 0xDE, .version = ._8501 });
    break :init list;
};

pub var current_cic: CicInfo = undefined;

pub fn init() void {
    var crc: u64 = 0;

    var i: u32 = 0x40;
    while (i < 0x1000) : (i += @sizeOf(u32)) {
        crc += memory.cart.rom.getWordPtr(i).*;
    }

    var cic_version = CicVersion.X102;
    switch (crc) {
        0x000000A5F80BF620 => cic_version = ._5101,
        0x000000D0027FDF31 => cic_version = .X101,
        0x000000CFFB631223 => cic_version = .X101,
        0x000000D057C85244 => cic_version = .X102,
        0x000000D6497E414B => cic_version = .X103,
        0x0000011A49F60E96 => cic_version = .X105,
        0x000000D6D5BE5580 => cic_version = .X106,
        0x000001053BC19870 => cic_version = ._5167,
        0x000000D2E53EF008 => cic_version = ._8303,
        0x000000D2E53EF39F => cic_version = ._8401,
        0x000000D2E53E5DDA => cic_version = ._8501,
        else => {
            cic_version = .X102;
            log.warn("Couldn't determine CIC type.", .{ });
        }
    }

    current_cic = cics.get(cic_version);
    log.info("Using {s} CIC Seed.", .{ @tagName(cic_version) });
}
