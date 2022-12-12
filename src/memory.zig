const std = @import("std");
const builtin = @import("builtin");

fn MemRange(comptime size_in_bytes: u32, comptime read_event: ?fn(u32) void, comptime write_event: ?fn(u32, u32, u32) bool) type {
    return struct {
        const Self = @This();

        buf: [size_in_bytes]u8 = [_]u8{0} ** size_in_bytes,

        inline fn writeWord(self: *Self, offset: u32, value: u32) void {
            var dst_ptr = @ptrCast(*align(1) u32, self.buf[offset..offset+3].ptr);
            dst_ptr.* = value;
        }

        inline fn readWord(self: *Self, offset: u32) u32 {
            var dst_ptr = @ptrCast(*align(1) u32, self.buf[offset..offset+3].ptr);
            return dst_ptr.*;
        }

        // The way this works is all reads and writes are only done to the 32-bit boundary, 
        // and there are no byteswaps involved. The way this is accomplished is using masking.
        // By calculating the mask we need based on the 32-bit boundary, we can simplify IO register events, 
        // and store everything in the CPUs native endian. (This is based off of mupen64plus's memory handling)
        inline fn writeWordMasked(self: *Self, offset: u32, value: u32, mask: u32) void {
            var dst_ptr = @ptrCast(*align(1) u32, self.buf[offset..offset+3].ptr);
            dst_ptr.* = (dst_ptr.* & ~mask) | (value & mask);
        }

        pub fn writeFromFile(self: *Self, file: std.fs.File) !usize {
            if (builtin.target.cpu.arch.endian() == .Little) {
                var index: usize = 0;
                var buffer_left: usize = self.buf.len;
                while (index != self.buf.len and buffer_left != 0) {
                    var u32_buffer: [4]u8 = undefined;
                    const amt = try file.read(u32_buffer[0..std.math.min(3, buffer_left-1)]);
                    if (amt == 0) break;

                    // Swap the Big-Endian bytes around.
                    var i: usize = 0;
                    while (i < amt) {
                        self.buf[index + (amt - i)] = u32_buffer[i];
                        i += 1;
                    }
                    index += amt;
                    buffer_left -= amt;
                }
                return index;
            } else {
                return file.readAll(&self.buf);
            }
        }

        pub inline fn writeAligned(self: *Self, offset: u32, value: anytype) void {
            var should_write: bool = true;
            var aligned_offset: u32 = offset;

            const T = @TypeOf(value);

            if (T == u32) {
                aligned_offset &= ~@as(u32, 0b11);

                if (write_event) |event| should_write = event(aligned_offset, value, 0xFFFFFFFF);
                if (should_write) self.writeWord(aligned_offset, @bitCast(u32, value));
            } else if (T == u64) {
                aligned_offset &= ~@as(u32, 0b111);

                self.writeAligned(aligned_offset, @truncate(u32, value >> 32));
                self.writeAligned(aligned_offset + 4, @truncate(u32, value));
            } else if (T == u8 or T == u16) {
                aligned_offset &= ~@as(u32, 0b11);

                var shift: u5 = undefined;
                if (T == u16) {
                    shift = ((@truncate(u5, offset) & 2) ^ 2) << 3;
                } else if (T == u8) {
                    shift = ((@truncate(u5, offset) & 3) ^ 3) << 3;
                }

                const value_mask: u32 = (1 << @typeInfo(T).Int.bits) - 1;

                var value_to_write: u32 = @as(u32, value) << shift;
                var mask: u32 = value_mask << shift;
                
                if (write_event) |event| should_write = event(aligned_offset, value_to_write, mask);
                if (should_write) self.writeWordMasked(aligned_offset, value_to_write, mask);
            } else {
                @compileError("Invalid type for IO write!");
            }
        }

        pub inline fn readAligned(self: *Self, comptime T: type, offset: u32) T {
            var aligned_offset: u32 = offset;

            if (T == u32) {
                aligned_offset &= ~@as(u32, 0b11);

                if (read_event) |event| event(aligned_offset);
                return self.readWord(aligned_offset);
            } else if (T == u64) {
                aligned_offset &= ~@as(u32, 0b111);
                return self.readAligned(u32, aligned_offset + 4) | (@intCast(u64, self.readAligned(u32, aligned_offset)) << 32);
            } else if (T == u8 or T == u16) {
                var shift: u5 = undefined;
                if (T == u16) {
                    aligned_offset &= ~@as(u32, 0b1);
                    shift = ((@truncate(u5, aligned_offset) & 2) ^ 2) << 3;
                } else if (T == u8) {
                    shift = ((@truncate(u5, aligned_offset) & 3) ^ 3) << 3;
                }

                aligned_offset &= ~@as(u32, 0b11);
                var result: u32 = self.readAligned(u32, aligned_offset);

                if (read_event) |event| event(aligned_offset);
                return @truncate(T, result >> shift);
            } else {
                @compileError("Invalid type for IO read!");
            }
        }
    };
}

pub const rdram = struct {
    pub const dram_size = 0x800000;
    pub var dram = MemRange(0x800000, null, null) { };

    pub const rdram_module_count: u8 = dram_size / 0x200000;
    pub const rdram_max_module_count: u8 = 8;
    pub const rdram_module_reg_count: u8 = 10;
    pub var reg_range = MemRange(@as(u32, rdram_max_module_count)*rdram_module_reg_count*@sizeOf(u32), null, null) { };

    pub const RegRangeOffset = enum(u8) {
        rdram_config_reg = 0x00,
        rdram_device_id_reg = 0x04,
        rdram_delay_reg = 0x08,
        rdram_mode_reg = 0x0c,
        rdram_ref_internal_reg = 0x10,
        rdram_ref_row_reg = 0x14,
        rdram_ras_interval_reg = 0x18,
        rdram_min_interval_reg = 0x1c,
        rdram_addr_select_reg = 0x20,
        rdram_device_manuf_reg = 0x24
    };

    pub fn getReg(offset: RegRangeOffset, module: u8) *align(1) u32 {
        return @ptrCast(*align(1) u32, 
            &reg_range.buf[(@as(u32, module) * (@as(u32, rdram_module_reg_count)*@sizeOf(u32))) + @enumToInt(offset)]);
    }

    fn init() void {
        // Initialize RDRAM registers. (values from mupen64plus)
        std.debug.assert(rdram_module_count <= rdram_max_module_count);
        var i: u8 = 0;
        while (i < rdram_module_count) : (i += 1) {
            getReg(.rdram_config_reg, i).* = 0xb5190010;
            getReg(.rdram_device_id_reg, i).* = 0x00000000;
            getReg(.rdram_delay_reg, i).* = 0x230b0223;
            getReg(.rdram_mode_reg, i).* = 0xc4c0c0c0;
            getReg(.rdram_ref_row_reg, i).* = 0x00000000;
            getReg(.rdram_min_interval_reg, i).* = 0x0040c0e0;
            getReg(.rdram_addr_select_reg, i).* = 0x00000000;
            getReg(.rdram_device_manuf_reg, i).* = 0x00000500;
        }
    }
};

pub const rcp = struct {
    pub const rsp = struct {
        pub var dmem = MemRange(0x1000, null, null) { };
        pub var imem = MemRange(0x1000, null, null) { };

        pub var reg_range_0 = MemRange(8*@sizeOf(u32), null, null) { };
        pub const RegRange0Offset = enum(u8) {
            sp_mem_addr_reg = 0x00,
            sp_dram_addr_reg = 0x04,
            sp_rd_len_reg = 0x08,
            sp_wr_len_reg = 0x0c,
            sp_status_reg = 0x10,
            sp_dma_full_reg = 0x14,
            sp_dma_busy_reg = 0x18,
            sp_semaphore_reg = 0x1c
        };

        pub var reg_range_1 = MemRange(2*@sizeOf(u32), null, null) { };
        pub const RegRange1Offset = enum(u8) {
            sp_pc_reg = 0x00,
            sp_ibist_reg = 0x04
        };
    };

    pub const rdp = struct {
        pub var cmd = MemRange(8*@sizeOf(u32), null, null) { };
        pub const CmdRangeOffset = enum(u8) {
            dpc_start_reg = 0x00,
            dpc_end_reg = 0x04,
            dpc_current_reg = 0x08,
            dpc_status_reg = 0x0C,
            dpc_clock_reg = 0x10,
            dpc_bufbusy_reg = 0x14,
            dpc_pipebusy_reg = 0x18,
            dpc_tmem_reg = 0x1C
        };

        pub var span = MemRange(4*@sizeOf(u32), null, null) { };
        pub const SpanRangeOffset = enum(u8) {
            dps_tbist_reg = 0x00,
            dps_test_mode_reg = 0x04,
            dps_buftest_addr_reg = 0x08,
            dps_buftest_data_reg = 0x0C
        };
    };

    pub const mi = struct {
        pub var reg_range = MemRange(4*@sizeOf(u32), null, null) { };
        pub const RegRangeOffset = enum(u8) {
            mi_init_mode_reg = 0x00,
            mi_version_reg = 0x04,
            mi_intr_reg = 0x08,
            mi_intr_mask_reg = 0x0C
        };
    };

    pub const vi = struct {
        pub var reg_range = MemRange(14*@sizeOf(u32), null, null) { };
        pub const RegRangeOffset = enum(u8) {
            vi_status_reg = 0x00,
            vi_origin_reg = 0x04,
            vi_width_reg = 0x08,
            vi_intr_reg = 0x0C,
            vi_current_reg = 0x10,
            vi_timing_reg = 0x14,
            vi_v_sync_reg = 0x18,
            vi_h_sync_reg = 0x1C,
            vi_leap_reg = 0x20,
            vi_h_start_reg = 0x24,
            vi_v_start_reg = 0x28,
            vi_v_burst_reg = 0x2C,
            vi_x_scale_reg = 0x30,
            vi_y_scale_reg = 0x34
        };
    };

    pub const ai = struct {
        pub var reg_range = MemRange(6*@sizeOf(u32), null, null) { };
        pub const RegRangeOffset = enum(u8) {
            ai_dram_addr_reg = 0x00,
            ai_len_reg = 0x04,
            ai_control_reg = 0x08,
            ai_status_reg = 0x0C,
            ai_dacrate_reg = 0x10,
            ai_bitrate_reg = 0x14
        };
    };

    pub const pi = struct {
        pub var reg_range = MemRange(13*@sizeOf(u32), null, null) { };
        pub const RegRangeOffset = enum(u8) {
            pi_dram_addr_reg = 0x00,
            pi_cart_addr_reg = 0x04,
            pi_rd_len_reg = 0x08,
            pi_wr_len_reg = 0x0C,
            pi_status_reg = 0x10,

            pi_bsd_dom1_lat_reg = 0x14,
            pi_bsd_dom1_pwd_reg = 0x18,
            pi_bsd_dom1_pgs_reg = 0x1C,
            pi_bsd_dom1_rls_reg = 0x20,

            pi_bsd_dom2_lat_reg = 0x24,
            pi_bsd_dom2_pwd_reg = 0x28,
            pi_bsd_dom2_pgs_reg = 0x2C,
            pi_bsd_dom2_rls_reg = 0x30,
        };
    };

    pub const ri = struct {
        pub var reg_range = MemRange(8*@sizeOf(u32), null, null) { };
        pub const RegRangeOffset = enum(u8) {
            ri_mode_reg = 0x00,
            ri_config_reg = 0x04,
            ri_current_load_reg = 0x08,
            ri_select_reg = 0x0C,
            ri_refresh_reg = 0x10,
            ri_latency_reg = 0x14,
            ri_rerror_reg = 0x18,
            ri_werror_reg = 0x1C
        };
    };

    pub const si = struct {
        pub var reg_range = MemRange(7*@sizeOf(u32), null, null) { };
        pub const RegRangeOffset = enum(u8) {
            si_dram_addr_reg = 0x00,
            si_pif_addr_rd64b_reg = 0x04,
            si_pif_addr_wr64b_reg = 0x10,
            si_status_reg = 0x18
        };
    };
};

pub const cart = struct {
    // My current design sort of accidentally avoids dynamic memory allocation 
    // (could mean that if I made the other parts of the emulator have some amount of abstraction, 
    // this emulator could run on bare metal),
    // so we just statically allocate the biggest size a cart could *technically* be. 
    // (More likely to be max 64MB, but 255MB is a fairly small size to allocate in RAM anyway)
    pub const max_cart_size = 255459327;
    pub var rom = MemRange(max_cart_size, null, null) { };
};

pub const pif = struct {
    pub var boot_rom = MemRange(2048, null, null) { };
    pub var ram = MemRange(64, null, null) { };
};

pub fn init() void {
    rdram.init();
}

// TESTS
test "Memory Range Read/Writes" {
    const first_write: u32 = 0xDEADBEEF;
    const first_offset: u32 = 0;

    const second_write: u64 = 0xDEADBEEFBADC0DE1;
    const second_offset: u32 = 8;

    const S = struct {
        var event_worked_properly: bool = false;

        pub fn readEvent(offset: u32) void {
            event_worked_properly = offset == first_offset & ~@as(u32, 0b11) or 
                offset == second_offset & ~@as(u32, 0b11) or offset == (second_offset + 4) & ~@as(u32, 0b11);
        }

        pub fn writeEvent(offset: u32, value: u32, mask: u32) bool {
            if (offset == first_offset & ~@as(u32, 0b11) and mask == 0xFFFFFFFF)
                event_worked_properly = value == first_write;
            if (offset == second_offset & ~@as(u32, 0b11) and mask == 0xFFFFFFFF)
                event_worked_properly = value == @truncate(u32, second_write >> 32);
            if (offset == (second_offset + 4) & ~@as(u32, 0b11) and mask == 0xFFFFFFFF)
                event_worked_properly = value == @truncate(u32, second_write);
            return true;
        }

        var test_range = MemRange(4*32, readEvent, writeEvent) { };

        fn testEventWorked() !void {
            try std.testing.expect(event_worked_properly);
            event_worked_properly = false;
        }

        pub fn testWrite(comptime T: type, offset: u32, value: T) !void {
            test_range.writeAligned(offset, value);
            try testEventWorked();

            const amount_of_bits = @typeInfo(T).Int.bits;

            try std.testing.expect(test_range.readAligned(T, offset) == value);
            try testEventWorked();

            if (amount_of_bits == 64) {
                 try std.testing.expect(test_range.readAligned(u32, offset) == @truncate(u32, value >> 32));
                 try testEventWorked();

                 try std.testing.expect(test_range.readAligned(u32, offset + 4) == @truncate(u32, value));
                 try testEventWorked();
            }

            if (amount_of_bits > 16) {
                comptime var i = 0;
                inline while (i < amount_of_bits / 16) : (i += 1) {
                    try std.testing.expect(test_range.readAligned(u16, offset + (i * 2)) == 
                        @truncate(u16, value >> (amount_of_bits-16) - (i * 16)));
                    try testEventWorked();
                }
            }

            if (amount_of_bits != 8) {
                comptime var i = 0;
                inline while (i < amount_of_bits / 8) : (i += 1) {
                    try std.testing.expect(test_range.readAligned(u8, offset + i) == 
                        @truncate(u8, value >> (amount_of_bits-8) - (i * 8)));
                    try testEventWorked();
                }
            }
        }
    };

    try S.testWrite(u32, first_offset, first_write);
    try S.testWrite(u64, second_offset, second_write);
}
