const std = @import("std");
const builtin = @import("builtin");

const cic = @import("cic.zig");
const cpu_bus = @import("cpu_bus.zig");

pub const MemRange = struct {
    const Self = @This();

    buf: []align(4) u8,
    allocator: std.mem.Allocator,

    write_event: ?*const fn(offset: u32, value: u32, mask: u32) bool = null,
    read_event: ?*const fn(offset: u32) void = null,

    fn readOnlyWriteEvent(_: u32, _: u32, _: u32) bool {
        return false;
    }

    pub fn init(allocator: std.mem.Allocator, size: usize) !Self {
        return Self {
            .buf = try allocator.alignedAlloc(u8, 4, size),
            .allocator = allocator
        };
    }

    pub fn initWithEvents(
        allocator: std.mem.Allocator, size: usize, 
        write_event: ?*const fn(u32, u32, u32) bool, read_event: ?*const fn(u32) void
    ) !Self {
        return Self {
            .buf = try allocator.alignedAlloc(u8, 4, size),
            .allocator = allocator,
            .write_event = write_event,
            .read_event = read_event
        };
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.buf);
    }

    pub fn clearToZero(self: *Self) void {
        std.mem.set(u8, self.buf, 0);
    }

    inline fn writeWord(self: *Self, offset: u32, value: u32) void {
        var dst_ptr = @ptrCast(*align(1) u32, self.buf[offset..offset+3].ptr);
        dst_ptr.* = value;
    }

    inline fn readWord(self: *Self, offset: u32) u32 {
        var dst_ptr = @ptrCast(*align(1) u32, self.buf[offset..offset+3].ptr);
        return dst_ptr.*;
    }

    pub fn getWordPtr(self: *Self, offset: u32) *align(1) u32 {
        std.debug.assert(offset & 0b11 == 0);
        return @ptrCast(*align(1) u32, self.buf[offset..offset+3].ptr);
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
            while (buffer_left >= 4) {
                var u32_buffer: [4]u8 = undefined;
                const amt = try file.read(u32_buffer[0..]);
                if (amt == 0) break;

                // Swap the Big-Endian bytes around.
                var i: usize = 0;
                while (i < amt) {
                    self.buf[index + ((amt-1) - i)] = u32_buffer[i];
                    i += 1;
                }
                index += amt;
                buffer_left -= amt;
            }
            return index;
        } else {
            return file.readAll(self.buf);
        }
    }

    pub inline fn writeAligned(self: *Self, offset: u32, value: anytype) void {
        var should_write: bool = true;
        var aligned_offset: u32 = offset;

        const T = @TypeOf(value);

        if (T == u32) {
            std.debug.assert(offset & 0b11 == 0);

            if (self.write_event) |event| should_write = event(aligned_offset, value, 0xFFFFFFFF);
            if (should_write) self.writeWord(aligned_offset, @bitCast(u32, value));
        } else if (T == u64) {
            std.debug.assert(offset & 0b111 == 0);

            if (self.write_event) |event| should_write = event(aligned_offset, @truncate(u32, value >> 32), 0xFFFFFFFF);
            if (should_write) self.writeWord(aligned_offset, @bitCast(u32, @truncate(u32, value >> 32)));
            should_write = true;

            if (self.write_event) |event| should_write = event(aligned_offset, @truncate(u32, value), 0xFFFFFFFF);
            if (should_write) self.writeWord(aligned_offset, @bitCast(u32, @truncate(u32, value)));
        } else if (T == u8 or T == u16) {
            var shift: u5 = undefined;
            if (T == u16) {
                std.debug.assert(offset & 0b1 == 0);
                shift = ((@truncate(u5, aligned_offset) & 2) ^ 2) << 3;
            } else if (T == u8) {
                shift = ((@truncate(u5, aligned_offset) & 3) ^ 3) << 3;
            }

            aligned_offset &= ~@as(u32, 0b11);
            const value_mask: u32 = (1 << @typeInfo(T).Int.bits) - 1;

            var value_to_write: u32 = @as(u32, value) << shift;
            var mask: u32 = value_mask << shift;
            
            if (self.write_event) |event| should_write = event(aligned_offset, value_to_write, mask);
            if (should_write) self.writeWordMasked(aligned_offset, value_to_write, mask);
        } else {
            @compileError("Invalid type for IO write!");
        }
    }

    pub inline fn readAligned(self: *Self, comptime T: type, offset: u32) T {
        var aligned_offset: u32 = offset;

        if (T == u32) {
            std.debug.assert(offset & 0b11 == 0);
            if (self.read_event) |event| event(aligned_offset);
            return self.readWord(aligned_offset);
        } else if (T == u64) {
            std.debug.assert(offset & 0b111 == 0);
            return self.readAligned(u32, aligned_offset + 4) | (@as(u64, self.readAligned(u32, aligned_offset)) << 32);
        } else if (T == u8 or T == u16) {
            var shift: u5 = undefined;
            if (T == u16) {
                std.debug.assert(offset & 0b1 == 0);
                shift = ((@truncate(u5, aligned_offset) & 2) ^ 2) << 3;
            } else if (T == u8) {
                shift = ((@truncate(u5, aligned_offset) & 3) ^ 3) << 3;
            }

            aligned_offset &= ~@as(u32, 0b11);
            var result: u32 = self.readAligned(u32, aligned_offset);

            if (self.read_event) |event| event(aligned_offset);
            return @truncate(T, result >> shift);
        } else {
            @compileError("Invalid type for IO read!");
        }
    }
};

var arena: std.heap.ArenaAllocator = undefined;
pub const MemoryError = error {
    InvalidPifBootrom
};

pub const rdram = struct {
    pub const dram_size = 0x800000;
    pub var dram: MemRange = undefined;

    pub const rdram_module_count: u8 = dram_size / 0x200000;
    pub const rdram_max_module_count: u8 = 8;
    pub const rdram_module_reg_count: u8 = 10;
    pub var reg_range: MemRange = undefined;

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
        return reg_range.getWordPtr(@enumToInt(offset) + (module * rdram_module_reg_count * @sizeOf(u32)));
    }

    fn init() !void {
        dram = try MemRange.init(arena.allocator(), dram_size);
        dram.clearToZero();
        reg_range = try MemRange.init(arena.allocator(), @as(u32, rdram_max_module_count)*rdram_module_reg_count*@sizeOf(u32));
        reg_range.clearToZero();

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
        pub var dmem: MemRange = undefined;
        pub var imem: MemRange = undefined;

        pub var reg_range_0: MemRange = undefined;
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

        pub var reg_range_1: MemRange = undefined;
        pub const RegRange1Offset = enum(u8) {
            sp_pc_reg = 0x00,
            sp_ibist_reg = 0x04
        };

        fn regRange0WriteEvent(offset: u32, value: u32, mask: u32) bool {
            const effective_value = value & mask;
            if (offset == @enumToInt(RegRange0Offset.sp_mem_addr_reg) or
                offset == @enumToInt(RegRange0Offset.sp_dram_addr_reg) or
                offset == @enumToInt(RegRange0Offset.sp_rd_len_reg) or
                offset == @enumToInt(RegRange0Offset.sp_wr_len_reg)) {
                return true;
            } else if (offset == @enumToInt(RegRange0Offset.sp_status_reg)) {
                var status_reg = reg_range_0.getWordPtr(offset);

                // Halt Bit
                if (effective_value & 0b01 != 0) { // Clear
                    status_reg.* &= ~@as(u32, 0b01);
                }
                if (effective_value & 0b10 != 0) { // Set
                    status_reg.* |= 0b01;
                }

                // Broke bit
                if (effective_value & 0b100 != 0) { // Clear
                    status_reg.* &= ~@as(u32, 0b10);
                }

                // MI interrupt bit
                if (effective_value & 0b01000 != 0) { // Clear
                    rcp.mi.getMiInterruptFlags().flags.sp = false;
                }
                if (effective_value & 0b10000 != 0) { // Set
                    rcp.mi.getMiInterruptFlags().flags.sp = true;
                }

                // Single-step bit
                if (effective_value & 0b0100000 != 0) { // Clear
                    status_reg.* &= ~@as(u32, 0b100000);
                }
                if (effective_value & 0b1000000 != 0) { // Set
                    status_reg.* |= 0b100000;
                }

                // Interrupt on break bit
                if (effective_value & 0b010000000 != 0) { // Clear
                    status_reg.* &= ~@as(u32, 0b1000000);
                }
                if (effective_value & 0b100000000 != 0) { // Set
                    status_reg.* |= 0b1000000;
                }

                // Signal 0 bit
                if (effective_value & 0b01000000000 != 0) { // Clear
                    status_reg.* &= ~@as(u32, 0b10000000);
                }
                if (effective_value & 0b10000000000 != 0) { // Set
                    status_reg.* |= 0b10000000;
                }

                // Signal 1 bit
                if (effective_value & 0b0100000000000 != 0) { // Clear
                    status_reg.* &= ~@as(u32, 0b100000000);
                }
                if (effective_value & 0b1000000000000 != 0) { // Set
                    status_reg.* |= 0b100000000;
                }

                // Signal 2 bit
                if (effective_value & 0b010000000000000 != 0) { // Clear
                    status_reg.* &= ~@as(u32, 0b1000000000);
                }
                if (effective_value & 0b100000000000000 != 0) { // Set
                    status_reg.* |= 0b1000000000;
                }

                // Signal 3 bit
                if (effective_value & 0b01000000000000000 != 0) { // Clear
                    status_reg.* &= ~@as(u32, 0b10000000000);
                }
                if (effective_value & 0b10000000000000000 != 0) { // Set
                    status_reg.* |= 0b10000000000;
                }

                // Signal 4 bit
                if (effective_value & 0b0100000000000000000 != 0) { // Clear
                    status_reg.* &= ~@as(u32, 0b100000000000);
                }
                if (effective_value & 0b1000000000000000000 != 0) { // Set
                    status_reg.* |= 0b100000000000;
                }

                // Signal 5 bit
                if (effective_value & 0b010000000000000000000 != 0) { // Clear
                    status_reg.* &= ~@as(u32, 0b1000000000000);
                }
                if (effective_value & 0b100000000000000000000 != 0) { // Set
                    status_reg.* |= 0b1000000000000;
                }

                // Signal 6 bit
                if (effective_value & 0b01000000000000000000000 != 0) { // Clear
                    status_reg.* &= ~@as(u32, 0b10000000000000);
                }
                if (effective_value & 0b10000000000000000000000 != 0) { // Set
                    status_reg.* |= 0b10000000000000;
                }

                // Signal 7 bit
                if (effective_value & 0b0100000000000000000000000 != 0) { // Clear
                    status_reg.* &= ~@as(u32, 0b100000000000000);
                }
                if (effective_value & 0b1000000000000000000000000 != 0) { // Set
                    status_reg.* |= 0b100000000000000;
                }
            }
            return false;
        }

        fn init() !void {
            dmem = try MemRange.init(arena.allocator(), 0x1000);
            imem = try MemRange.init(arena.allocator(), 0x1000);

            reg_range_0 = try MemRange.initWithEvents(arena.allocator(), 8*@sizeOf(u32), regRange0WriteEvent, null);
            reg_range_0.clearToZero();
            reg_range_1 = try MemRange.init(arena.allocator(), 2*@sizeOf(u32));
            reg_range_1.clearToZero();

            reg_range_0.getWordPtr(@enumToInt(RegRange0Offset.sp_status_reg)).* = 0x00000001;
        }
    };

    pub const rdp = struct {
        pub var cmd: MemRange = undefined;
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

        pub var span: MemRange = undefined;
        pub const SpanRangeOffset = enum(u8) {
            dps_tbist_reg = 0x00,
            dps_test_mode_reg = 0x04,
            dps_buftest_addr_reg = 0x08,
            dps_buftest_data_reg = 0x0C
        };

        fn init() !void {
            cmd = try MemRange.init(arena.allocator(), 8*@sizeOf(u32));
            cmd.clearToZero();
            span = try MemRange.init(arena.allocator(), 4*@sizeOf(u32));
            span.clearToZero();
        }
    };

    pub const mi = struct {
        pub var reg_range: MemRange = undefined;
        pub const RegRangeOffset = enum(u8) {
            mi_init_mode_reg = 0x00,
            mi_version_reg = 0x04,
            mi_intr_reg = 0x08,
            mi_intr_mask_reg = 0x0C
        };

        pub const MiInterruptBits = packed union {
            flags: packed struct(u32) {
                sp: bool,
                si: bool,
                ai: bool,
                vi: bool,
                pi: bool,
                dp: bool,
                _always_zero: u26
            },
            bits: u32
        };

        pub fn getMiInterruptFlags() *align(1) MiInterruptBits {
            return @ptrCast(*align(1) MiInterruptBits, reg_range.getWordPtr(@enumToInt(RegRangeOffset.mi_intr_reg)));
        }

        pub fn getMiMaskFlags() *align(1) MiInterruptBits {
            return @ptrCast(*align(1) MiInterruptBits, reg_range.getWordPtr(@enumToInt(RegRangeOffset.mi_intr_mask_reg)));
        }

        fn miWriteEvent(offset: u32, value: u32, mask: u32) bool {
            const effective_value = value & mask;
            if (offset == @enumToInt(RegRangeOffset.mi_intr_mask_reg)) {
                // SP mask
                if (effective_value & 0b01 > 0) { // Clear
                    getMiMaskFlags().flags.sp = false;
                }
                if (effective_value & 0b10 > 0) { // Set
                    getMiMaskFlags().flags.sp = true;
                }

                // SI mask
                if (effective_value & 0b0100 > 0) { // Clear
                    getMiMaskFlags().flags.si = false;
                }
                if (effective_value & 0b1000 > 0) { // Set
                    getMiMaskFlags().flags.si = true;
                }

                // AI mask
                if (effective_value & 0b010000 > 0) { // Clear
                    getMiMaskFlags().flags.ai = false;
                }
                if (effective_value & 0b100000 > 0) { // Set
                    getMiMaskFlags().flags.ai = true;
                }

                // VI mask
                if (effective_value & 0b01000000 > 0) { // Clear
                    getMiMaskFlags().flags.vi = false;
                }
                if (effective_value & 0b10000000 > 0) { // Set
                    getMiMaskFlags().flags.vi = true;
                }

                // PI mask
                if (effective_value & 0b0100000000 > 0) { // Clear
                    getMiMaskFlags().flags.pi = false;
                }
                if (effective_value & 0b1000000000 > 0) { // Set
                    getMiMaskFlags().flags.pi = true;
                }

                // DP mask
                if (effective_value & 0b010000000000 > 0) { // Clear
                    getMiMaskFlags().flags.dp = false;
                }
                if (effective_value & 0b100000000000 > 0) { // Set
                    getMiMaskFlags().flags.dp = true;
                }
            }
            return false;
        }

        fn init() !void {
            reg_range = try MemRange.initWithEvents(arena.allocator(), 4*@sizeOf(u32), miWriteEvent, null);
            reg_range.clearToZero();
            reg_range.getWordPtr(@enumToInt(RegRangeOffset.mi_version_reg)).* = 0x02020102;
        }
    };

    pub const vi = struct {
        pub var reg_range: MemRange = undefined;
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

        pub const Bpp = enum(u2) {
            blank = 0,
            _reserved = 1,
            _16bit = 2,
            _32bit = 3
        };

        pub const AaMode = enum(u2) {
            aa_resample_always_fetch_extra_lines = 0,
            aa_resample_fetch_extra_lines_if_needed = 1,
            resample_only = 2,
            disabled = 3
        };

        pub const ViStatusRegister = packed struct(u32) {
            bpp: Bpp,
            gamma_dither_enable: bool,
            gamma_enable: bool,
            divot_enable: bool,
            vbus_clock_enable: bool,
            interlace_enable: bool,
            test_mode: bool,
            aa_mode: AaMode,
            _undefined: bool,
            kill_we: bool,
            pixel_advance: u4,
            dither_filter_enable: bool,
            _unused: u15,

            pub fn getPixelSize(self: @This()) u5 {
                return switch (self.bpp) {
                    .blank => 0,
                    ._reserved => 0,
                    ._16bit => @sizeOf(u16),
                    ._32bit => @sizeOf(u32)
                };
            }
        };

        fn viWriteEvent(offset: u32, value: u32, mask: u32) bool {
            _ = value;
            _ = mask;
            if (offset == @enumToInt(RegRangeOffset.vi_current_reg)) {
                rcp.mi.getMiInterruptFlags().flags.vi = false;
                return false;
            }
            return true;
        }

        pub fn getViStatusFlags() *align(1) ViStatusRegister {
            return @ptrCast(*align(1) ViStatusRegister, reg_range.getWordPtr(
                @enumToInt(RegRangeOffset.vi_status_reg)));
        }

        fn init() !void {
            reg_range = try MemRange.initWithEvents(arena.allocator(), 14*@sizeOf(u32), viWriteEvent, null);
            reg_range.clearToZero();
        }
    };

    pub const ai = struct {
        pub var reg_range: MemRange = undefined;
        pub const RegRangeOffset = enum(u8) {
            ai_dram_addr_reg = 0x00,
            ai_len_reg = 0x04,
            ai_control_reg = 0x08,
            ai_status_reg = 0x0C,
            ai_dacrate_reg = 0x10,
            ai_bitrate_reg = 0x14
        };

        fn init() !void {
            reg_range = try MemRange.init(arena.allocator(), 6*@sizeOf(u32));
            reg_range.clearToZero();
        }
    };

    pub const pi = struct {
        pub var reg_range: MemRange = undefined;
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

        fn piWriteEvent(offset: u32, value: u32, mask: u32) bool {
            const effective_value = value & mask;
            if (offset == @enumToInt(RegRangeOffset.pi_status_reg)) {
                if (effective_value & 0b01 != 0) { // Reset Controller
                    // TODO: PIF controller stuff.
                }

                if (effective_value & 0b10 != 0) { // Clear MI Interrupt bit
                    rcp.mi.getMiInterruptFlags().flags.pi = false;
                }
                return false;
            } else if (offset == @enumToInt(RegRangeOffset.pi_wr_len_reg)) {
                // TODO: Delay PI DMA
                var length = effective_value+1;
                var dram_offset = (reg_range.getWordPtr(@enumToInt(RegRangeOffset.pi_dram_addr_reg)).* & 0x1FFFFFFF) - cpu_bus.rdram_base_addr;
                var cart_offset = (reg_range.getWordPtr(@enumToInt(RegRangeOffset.pi_cart_addr_reg)).* & 0x1FFFFFFF) - cpu_bus.cart_dom1_addr2_base_addr;
                std.mem.copy(u8, rdram.dram.buf[dram_offset..dram_offset+length], cart.rom.buf[cart_offset..cart_offset+length]);
            }
            return true;
        }

        fn init() !void {
            reg_range = try MemRange.initWithEvents(arena.allocator(), 13*@sizeOf(u32), piWriteEvent, null);
            reg_range.clearToZero();
        }
    };

    pub const ri = struct {
        pub var reg_range: MemRange = undefined;
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

        fn init() !void {
            reg_range = try MemRange.init(arena.allocator(), 8*@sizeOf(u32));
            reg_range.clearToZero();
        }
    };

    pub const si = struct {
        pub var reg_range: MemRange = undefined;
        pub const RegRangeOffset = enum(u8) {
            si_dram_addr_reg = 0x00,
            si_pif_addr_rd64b_reg = 0x04,
            si_pif_addr_wr64b_reg = 0x10,
            si_status_reg = 0x18
        };

        fn init() !void {
            reg_range = try MemRange.init(arena.allocator(), 4*@sizeOf(u32));
            reg_range.clearToZero();
        }
    };

    fn init() !void {
        try rsp.init();
        try rdp.init();
        
        try mi.init();
        try vi.init();
        try ai.init();
        try pi.init();
        try ri.init();
        try si.init();
    }
};

pub const cart = struct {
    pub const max_cart_size = 255459327;
    pub var rom: MemRange = undefined;

    pub fn init(rom_file: std.fs.File) !void {
        var size = std.math.min(max_cart_size, try rom_file.getEndPos());
        rom = try MemRange.initWithEvents(arena.allocator(), size, MemRange.readOnlyWriteEvent, null);
        _ = try rom.writeFromFile(rom_file);

        cic.init();
    }
};

pub const pif = struct {
    pub var boot_rom: MemRange = undefined;
    pub var ram: MemRange = undefined;

    pub fn init(boot_rom_file: ?std.fs.File) !void {
        boot_rom = try MemRange.initWithEvents(arena.allocator(), 2048, MemRange.readOnlyWriteEvent, null);
        if (boot_rom_file) |file| {
            if (try file.getEndPos() < 2048) {
                return error.InvalidPifBootrom;
            }
            _ = try boot_rom.writeFromFile(file);
        }

        ram = try MemRange.init(arena.allocator(), 64);
        ram.clearToZero();

        // HACK: for allowing pifbootrom execution (from mupen64plus device/pif/pif.c)
        var rom_type: u32 = switch (cic.current_cic.version == ._8303 or cic.current_cic.version == ._8401 or cic.current_cic.version == ._8501) {
            true => 1,
            false => 0
        };
        var s7: u32 = 0;
        var reset_type: u32 = 0;

        ram.getWordPtr(0x24).* = 
            (((rom_type & 0x1) << 19)
                | ((s7 & 0x1) << 18)
                | ((reset_type & 0x1) << 17)
                | (@as(u32, cic.current_cic.seed) << 8)
                | 0x3f);
    }
};

pub fn init() !void {
    arena = std.heap.ArenaAllocator.init(std.heap.raw_c_allocator);

    try rdram.init();
    try rcp.init();
}

pub fn deinit() void {
    arena.deinit();
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

        var test_range: MemRange = undefined;

        fn init() !void {
            test_range = try MemRange.initWithEvents(std.testing.allocator, 4*32, writeEvent, readEvent);
        }

        fn deinit() void {
            test_range.deinit();
        }

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

    try S.init();
    try S.testWrite(u32, first_offset, first_write);
    try S.testWrite(u64, second_offset, second_write);

    S.deinit();
}
