const std = @import("std");
const cpu_bus = @import("cpu_bus.zig");
const memory = @import("memory.zig");
const system = @import("system.zig");
const c = @import("c.zig");

const log = std.log.scoped(.r4300);

pub var gpr = [_]u64{0} ** 32;

pub var fpr = [_]u64{0} ** 32;

pub var fcr0: u32 = 0;
pub var fcr31: u32 = 0;

pub const FloatingPointRoundingMode = enum(u2) {
    nearest = 0b00,
    toward_zero = 0b01,
    positive_inf = 0b10,
    negative_inf = 0b11
};

pub const FloatingPointControlRegister31 = packed struct(u32) {
    rounding_mode: FloatingPointRoundingMode,
    flags: packed union {
        flags: packed struct {
            inexact_operation: bool,
            underflow: bool,
            overflow: bool,
            division_by_zero: bool,
            invalid_operation: bool
        },
        bits: u5
    },
    enable: packed union {
        flags: packed struct {
            inexact_operation: bool,
            underflow: bool,
            overflow: bool,
            division_by_zero: bool,
            invalid_operation: bool
        },
        bits: u5
    },
    cause: packed union {
        flags: packed struct {
            inexact_operation: bool,
            underflow: bool,
            overflow: bool,
            division_by_zero: bool,
            invalid_operation: bool,
            unimplemented_operation: bool
        },
        bits: u6
    },
    _padding0: u5,
    condition: bool,
    fs_enable: bool,
    _padding1: u7
};

pub fn getFcr31Flags() *FloatingPointControlRegister31 {
    return @ptrCast(*FloatingPointControlRegister31, &fcr31);
}

pub var cp1_coc: bool = false;

pub var hi: u64 = 0;
pub var lo: u64 = 0;

pub var ll_bit: bool = false;

pub var cp0 = [_]u32{0} ** 32;
pub const Cop0Register = enum(u8) {
    index,
    random,
    entrylo0,
    entrylo1,
    context,
    pagemask,
    wired,
    _reserved0,
    bad_vaddr,
    count,
    entryhi,
    compare,
    status,
    cause,
    epc,
    previd,
    config,
    _reserved1, // (Technically lladdr, but load linked doesn't exist on the N64)
    watchlo,
    watchhi,
    _reserved2, // (Technically XContext, but the N64 can't go into 64-bit mode)
    _reserved3,
    _reserved4,
    _reserved5,
    _reserved6,
    _reserved7,
    _reserved8,
    _reserved9,
    taglo,
    taghi,
    errorepc,
    _reserved10
};
const cp0_write_mask = [32]u32 {
    0x0000003F, // index
    0x00000000, // random
    0x3FFFFFFF, // entrylo0
    0x3FFFFFFF, // entrylo1
    0xFF800000, // context (also has special handling in mtc0)
    0x01FFE000, // pagemask
    0xFFFFFFFF, // wired (also has special handling in mtc0)
    0x00000000, // _reserved0
    0x00000000, // bad_vaddr
    0x00000000, // count
    0xFFFFE0FF, // entryhi
    0xFFFFFFFF, // compare (also has special handling in mtc0)
    0xFFFFFF1F, // status
    0x00000300, // cause (also has special handling in mtc0)
    0xFFFFFFFF, // epc
    0x00000000, // previd
    0xFFFFFFFF, // config
    0x00000000, // _reserved1
    0xFFFFFFFF, // watchlo
    0xFFFFFFFF, // watchhi
    0x00000000, // _reserved2
    0x00000000, // _reserved3
    0x00000000, // _reserved4
    0x00000000, // _reserved5
    0x00000000, // _reserved6
    0x00000000, // _reserved7
    0x00000000, // _reserved8
    0x00000000, // _reserved9
    0x0FFFFFC0, // taglo
    0x00000000, // taghi (also has special handling in mtc0)
    0xFFFFFFFF, // errorepc
    0x00000000, // _reserved10
};

pub const InterruptBits = packed union { 
    flags: packed struct(u8) {
        software_interrupt0: bool,
        software_interrupt1: bool,
        rcp: bool,
        _unused: u4,
        timer_interrupt: bool
    },
    bits: u8
};

pub const Cop0StatusRegister = packed struct(u32) {
    pub const PrivilegeMode = enum(u2) {
        user = 0b10,
        supervisor = 0b01,
        kernel = 0b00
    };

    interrupt_enable: bool,
    is_exception: bool,
    is_error: bool,

    // None of these should be used on the N64
    privilege_level: PrivilegeMode,
    is_64bit_user_address_space: bool,
    is_64bit_supervisor_address_space: bool,
    is_64bit_kernel_address_space: bool,
    //

    interrupt_mask: InterruptBits,
    diagnostic_field: packed struct(u9) {
        de: bool,
        ce: bool,
        cp0_condition_bit: bool,
        _always_zero_0: bool,
        soft_reset_or_nmi: bool,
        tlb_shutdown: bool,
        bootstrap_exception_vector: bool,
        _always_zero_1: bool,
        instruction_trace_enable: bool
    },

    reverse_endian_user: bool,
    floating_point_registers_enable: bool,
    low_power_mode: bool,

    coprocessor_enable: packed struct(u4) {
        cop0_enable: bool,
        cop1_enable: bool,
        cop2_enable: bool,
        cop3_enable: bool
    },
};

pub fn getStatusFlags() *Cop0StatusRegister {
    return @ptrCast(*Cop0StatusRegister, &cp0[@enumToInt(Cop0Register.status)]);
}

pub const Cop0ConfigRegister = packed struct(u32) {
    kseg0_flags: u3,
    cu: bool,
    _data0: u11,
    big_endian_enable: bool,
    _data1: u8,
    transfer_data_pattern: u4,
    operating_frequency_ratio: u3,
    _always_zero: u1
};

pub fn getConfigFlags() *Cop0ConfigRegister {
    return @ptrCast(*Cop0ConfigRegister, &cp0[@enumToInt(Cop0Register.config)]);
}

pub const ExcCode = enum(u5) {
    interrupt = 0,
    tlb_modification = 1,
    tlb_miss_load = 2,
    tlb_miss_store = 3,
    address_error_load = 4,
    address_error_store = 5,
    bus_error_instruction_fetch = 6,
    bus_error_load_store = 7,
    syscall = 8,
    breakpoint = 9,
    reserved_instruction = 10,
    coprocessor_unusable = 11,
    overflow = 12,
    trap = 13,
    floating_point = 15,
    watch = 23
};

pub const Cop0CauseRegister = packed struct(u32) {
    _always_zero_0: u2,
    exc_code: ExcCode,
    _always_zero_1: u1,
    interrupts_pending: InterruptBits,
    _always_zero_2: u12,
    coprocessor_number: u2,
    _always_zero_3: u1,
    branch_delay_slot: bool
};

pub fn getCauseFlags() *Cop0CauseRegister {
    return @ptrCast(*Cop0CauseRegister, &cp0[@enumToInt(Cop0Register.cause)]);
}

pub var inst_count: u64 = 0;

pub const Instruction = packed struct(u32) {
    pub const SpecialFunction = enum(u6) {
        add    = 0b100000,
        addu   = 0b100001,
        _and   = 0b100100,
        _break = 0b001101,
        dadd   = 0b101100,
        daddu  = 0b101101,
        ddiv   = 0b011110,
        ddivu  = 0b011111,
        div    = 0b011010,
        divu   = 0b011011,
        dmult  = 0b011100,
        dmultu = 0b011101,
        dsll   = 0b111000,
        dsllv  = 0b010100,
        dsll32 = 0b111100,
        dsra   = 0b111011,
        dsrav  = 0b010111,
        dsra32 = 0b111111,
        dsrl   = 0b111010,
        dsrlv  = 0b010110,
        dsrl32 = 0b111110,
        dsub   = 0b101110,
        dsubu  = 0b101111,
        jalr   = 0b001001,
        jr     = 0b001000,
        mfhi   = 0b010000,
        mflo   = 0b010010,
        mthi   = 0b010001,
        mtlo   = 0b010011,
        mult   = 0b011000,
        multu  = 0b011001,
        nor    = 0b100111,
        _or    = 0b100101,
        sll    = 0b000000,
        sllv   = 0b000100,
        slt    = 0b101010,
        sltu   = 0b101011,
        sra    = 0b000011,
        srav   = 0b000111,
        srl    = 0b000010,
        srlv   = 0b000110,
        sub    = 0b100010,
        subu   = 0b100011,
        sync   = 0b001111,
        xor    = 0b100110
    };

    pub const CopSubOpcode = enum(u5) {
        bc = 0b01000, // Branch Conditional
        cf = 0b00010, // Move Control From
        ct = 0b00110, // Move Control To
        mf = 0b00000, // Move From
        mt = 0b00100  // Move To
    };

    pub const CopBranchCondition = enum(u5) {
        bcf  = 0b00000,
        bcfl = 0b00010,
        bct  = 0b00001,
        bctl = 0b00011
    };

    pub const RegimmSubOpcode = enum(u5) {
        bgez    = 0b00001,
        bgezal  = 0b10001,
        bgezall = 0b10011,
        bgezl   = 0b00011,
        bltz    = 0b00000,
        bltzal  = 0b10000,
        bltzall = 0b10010,
        bltzl   = 0b00010,
    };

    pub const Cop1Function = enum(u6) {
    };

    pub const Cop0Function = enum(u6) {
        eret = 0b011000
    };

    data: packed union {
        i_type: packed struct {
            immediate: u16,
            rt: u5,
            rs: u5
        },
        j_type: packed struct {
            target: u26
        },
        r_type: packed struct {
            function: SpecialFunction,
            sa: u5,
            rd: u5,
            rt: u5,
            rs: u5
        },
        r_type_cop: packed struct {
            _: u11,
            rd: u5,
            rt: u5,
            sub_opcode: CopSubOpcode
        },
        r_type_cop1: packed struct {
            function: Cop1Function,
            fd: u5,
            fs: u5,
            rt: u5,
            fmt: u5
        },
        i_type_mem: packed struct {
            offset: i16,
            rt: u5,
            base: u5
        },
        i_type_branch: packed struct {
            offset: i16,
            rt: u5,
            rs: u5
        },
        i_type_regimm_branch: packed struct {
            offset: i16,
            sub_opcode: RegimmSubOpcode,
            rs: u5
        },
        i_type_cop_branch: packed struct {
            offset: i16,
            condition: CopBranchCondition,
            sub_opcode: CopSubOpcode
        },
        cop0_generic: packed struct {
            function: Cop0Function,
            _unused: u20
        }
    },
    opcode: u6
};

// On the N64 the PC is really only 32-bit, even though the processor is technically capable of a 64-bit mode.
pub var pc: u32 = 0;

// Stores the PC before being changed / incremented by the ran instruction. 
// Used for setting the EPC registers.
var last_pc: u32 = 0;

var branch_target: ?i32 = null;
var jump_target: ?u32 = null;
var jump_reg_target: ?u32 = null;
var is_delay_slot: bool = false;

pub const CpuError = error {
    UnknownInstruction,
    NotImplemented,
    InvalidInstruction
};

inline fn overrideBranch() void {
    branch_target = null;
    jump_target = null;
    jump_reg_target = null;
    is_delay_slot = false;
}

pub inline fn virtualToPhysical(vAddr: u32) u32 {
    // TODO: Implement the TLB.
    return vAddr & 0x1FFFFFFF;
}

fn runHlePif() void {
    cp0[@enumToInt(Cop0Register.status)] = 0x000000E0; // PIF rom overwrites it (three bits are always set)
    // Enable Coprocessor 1.
    getStatusFlags().coprocessor_enable.cop1_enable = true;
    getStatusFlags().floating_point_registers_enable = true; // Enable all floating-point registers.

    getConfigFlags().kseg0_flags = 0b011; // Sets kseg0 to be uncached.
    getConfigFlags().cu = false; // An unused bit that can be written to and read from.

    cp0[@enumToInt(Cop0Register.errorepc)] = 0xFFFFFFFF;
    cp0[@enumToInt(Cop0Register.previd)] = 0xB22;
    cp0[@enumToInt(Cop0Register.epc)] = 0xFFFFFFFF;

    // halt the RSP, clear interrupt
    memory.rcp.rsp.reg_range_0.writeAligned(
        @enumToInt(memory.rcp.rsp.RegRange0Offset.sp_status_reg), @as(u32, 0b1010)
    );

    // reset the PI, clear interrupt
    memory.rcp.pi.reg_range.writeAligned(
        @enumToInt(memory.rcp.pi.RegRangeOffset.pi_status_reg), @as(u32, 0b11)
    );
    
    memory.rcp.vi.reg_range.writeAligned(
        @enumToInt(memory.rcp.vi.RegRangeOffset.vi_intr_reg), @as(u32, 1023)
    );
    memory.rcp.vi.reg_range.writeAligned(
        @enumToInt(memory.rcp.vi.RegRangeOffset.vi_current_reg), @as(u32, 0)
    );
    memory.rcp.vi.reg_range.writeAligned(
        @enumToInt(memory.rcp.vi.RegRangeOffset.vi_h_start_reg), @as(u32, 0)
    );

    memory.rcp.ai.reg_range.writeAligned(
        @enumToInt(memory.rcp.ai.RegRangeOffset.ai_dram_addr_reg), @as(u32, 0)
    );
    memory.rcp.ai.reg_range.writeAligned(
        @enumToInt(memory.rcp.ai.RegRangeOffset.ai_len_reg), @as(u32, 0)
    );

    // Parse pif24
    var pif24 = memory.pif.ram.readAligned(u32, 0x24);
    var rom_type = (pif24 >> 19) & 1;
    var s7 = (pif24 >> 18) & 1;
    var reset_type = (pif24 >> 17) & 1;
    var seed = (pif24 >> 8) & 0xFF;
    var tv_type: u32 = @enumToInt(system.config.tv_type);
    gpr[19] = rom_type;
    gpr[20] = tv_type;
    gpr[21] = reset_type;
    gpr[22] = seed;
    gpr[23] = s7;
    
    // Write the cart timing configuration
    var bsd_dom1_config = memory.cart.rom.readAligned(u32, 0);
    memory.rcp.pi.reg_range.writeAligned(
        @enumToInt(memory.rcp.pi.RegRangeOffset.pi_bsd_dom1_lat_reg), 
        bsd_dom1_config & 0xFF
    );
    memory.rcp.pi.reg_range.writeAligned(
        @enumToInt(memory.rcp.pi.RegRangeOffset.pi_bsd_dom1_pwd_reg), 
        (bsd_dom1_config >> 8) & 0xFF
    );
    memory.rcp.pi.reg_range.writeAligned(
        @enumToInt(memory.rcp.pi.RegRangeOffset.pi_bsd_dom1_pgs_reg), 
        (bsd_dom1_config >> 16) & 0x0F
    );
    memory.rcp.pi.reg_range.writeAligned(
        @enumToInt(memory.rcp.pi.RegRangeOffset.pi_bsd_dom1_rls_reg), 
        (bsd_dom1_config >> 20) & 0b11
    );

    // Copy IPL3 bootcode to RSP DMEM
    std.mem.copy(u8, memory.rcp.rsp.dmem.buf[0x40..], memory.cart.rom.buf[0x40..0x1000]);

    // Required by CIC x105
    memory.rcp.rsp.imem.writeAligned(0x00, @as(u32, 0x3c0dbfc0));
    memory.rcp.rsp.imem.writeAligned(0x04, @as(u32, 0x8da807fc));
    memory.rcp.rsp.imem.writeAligned(0x08, @as(u32, 0x25ad07c0));
    memory.rcp.rsp.imem.writeAligned(0x0C, @as(u32, 0x31080080));
    memory.rcp.rsp.imem.writeAligned(0x10, @as(u32, 0x5500fffc));
    memory.rcp.rsp.imem.writeAligned(0x14, @as(u32, 0x3c0dbfc0));
    memory.rcp.rsp.imem.writeAligned(0x18, @as(u32, 0x8da80024));
    memory.rcp.rsp.imem.writeAligned(0x1C, @as(u32, 0x3c0bb000));
    gpr[11] = 0xFFFFFFFFA0000000 + @as(u64, cpu_bus.sp_dmem_base_addr) + 0x40;
    gpr[29] = 0xFFFFFFFFA0000000 + @as(u64, cpu_bus.sp_imem_base_addr) + 0xff0;
    gpr[31] = 0xFFFFFFFFA0000000 + @as(u64, cpu_bus.sp_imem_base_addr) + 0x550;

    pc = 0xA0000000 + cpu_bus.sp_dmem_base_addr + 0x40;
}

pub fn init() void {
    cop0.raiseColdReset();
}

pub fn step() CpuError!void {
    gpr[0] = 0;

    // TODO: Emulate this better?
    // Unfortunately there's no way to accurately emulate the Cycle Count register
    // without making the emulated CPU be cycle-accurate (a la Cen64)
    // but there could be a better way to do this depending on how games want to use it.
    //
    // In any case, for non-cycle-accurate emulators, this is indeed a hard thing to emulate
    // and I've noticed some emulators even change the amount to increment 
    // based off the game manually as a hack!!! (something I'd like to avoid)
    //
    // Anyway, for now this should work fine.
    const cp0_count = @truncate(u32, inst_count);
    cp0[@enumToInt(Cop0Register.count)] = cp0_count;
    if (cp0_count == cp0[@enumToInt(Cop0Register.compare)]) {
        getCauseFlags().interrupts_pending.flags.timer_interrupt = true;
    }

    // Cause an RCP interrupt if we have a pending non-masked MI interrupt.
    getCauseFlags().interrupts_pending.flags.rcp = 
        (memory.rcp.mi.reg_range.getWordPtr(
            @enumToInt(memory.rcp.mi.RegRangeOffset.mi_intr_reg)).* & 0b111111) & 
        (memory.rcp.mi.reg_range.getWordPtr(
            @enumToInt(memory.rcp.mi.RegRangeOffset.mi_intr_mask_reg)).* & 0b111111) > 0;

    var instruction_ptr = cpu_bus.getWordPtr(virtualToPhysical(pc));
    std.debug.assert(instruction_ptr != null);
    last_pc = pc;
    try interpreter.executeInstruction(@ptrCast(*align(1) Instruction, instruction_ptr.?).*);
    inst_count +%= 1; // Shouldn't roll over in about 6,000 years
    // (in contrast to a 32-bit number which will roll over in 40 seconds, both numbers assuming 93.75MHz)

    // Handle interrupts.
    if (getStatusFlags().interrupt_enable and !(getStatusFlags().is_exception or getStatusFlags().is_error)) {
        if (getCauseFlags().interrupts_pending.bits & getStatusFlags().interrupt_mask.bits > 0) {
            cop0.raiseCommonException(.interrupt);
        }
    }

    // Handle exceptions (including interrupt exceptions).
    if (cop0.current_exception != null) {
        cop0.jumpToExceptionHandler();
        cop0.current_exception = null;
    }

    // Handle jumps and branches.
    if ((jump_target != null or branch_target != null or jump_reg_target != null) and !is_delay_slot) {
        is_delay_slot = true;
    } else if (jump_target != null and is_delay_slot) {
        pc = (pc & 0xF0000000) | jump_target.?;
        jump_target = null;
        is_delay_slot = false;
    } else if (jump_reg_target != null and is_delay_slot) {
        pc = jump_reg_target.?;
        jump_reg_target = null;
        is_delay_slot = false;
    } else if (branch_target != null and is_delay_slot) {
        pc = (pc - 4) +% @bitCast(u32, branch_target.?); // Relative to delay slot instruction.
        branch_target = null;
        is_delay_slot = false;
    }
}

pub const cop0 = struct {
    // This is sorted in order of priority, with the top exceptions 
    // taking priority over the lower exceptions if happening on the same CPU cycle.
    pub const ExceptionType = enum(u8) {
        cold_reset,
        soft_reset_or_nmi,
        address_error_instruction_fetch,
        tlb_miss_instruction_fetch,
        tlb_invalid_instruction_fetch,
        bus_error_instruction_fetch,
        syscall,
        breakpoint,
        coprocessor_unusable,
        reserved_instruction,
        trap,
        overflow,
        floating_point,
        address_error_load,
        address_error_store,
        tlb_miss_load,
        tlb_miss_store,
        tlb_modification,
        watch, // TODO: Emulate postponing behaviour when EXL is set. (Probably not necessary though)
        bus_error_load_store,
        interrupt
    };
    pub var current_exception: ?ExceptionType = null;

    inline fn typeToExcCode(t: ExceptionType) !ExcCode {
        return switch(t) {
            .address_error_instruction_fetch => .address_error_load,
            .tlb_miss_instruction_fetch => .tlb_miss_load,
            .tlb_invalid_instruction_fetch => .tlb_miss_load,
            .bus_error_instruction_fetch => .bus_error_instruction_fetch,
            .syscall => .syscall,
            .breakpoint => .breakpoint,
            .coprocessor_unusable => .coprocessor_unusable,
            .reserved_instruction => .reserved_instruction,
            .trap => .trap,
            .overflow => .overflow,
            .floating_point => .floating_point,
            .address_error_load => .address_error_load,
            .address_error_store => .address_error_store,
            .tlb_miss_load => .tlb_miss_load,
            .tlb_miss_store => .tlb_miss_store,
            .tlb_modification => .tlb_modification,
            .watch => .watch,
            .bus_error_load_store => .bus_error_load_store,
            .interrupt => .interrupt,
            else => error.InvalidArgs
        };
    }

    pub fn raiseColdReset() void {
        if (current_exception != null and @enumToInt(ExceptionType.cold_reset) > @enumToInt(current_exception.?)) return;

        getConfigFlags().big_endian_enable = true;
        getConfigFlags().operating_frequency_ratio = 0b111;
        getConfigFlags()._data0 = 0b11001000110;
        getConfigFlags()._data1 = 0b00000110;
        getConfigFlags().transfer_data_pattern = 0;

        cp0[@enumToInt(Cop0Register.random)] = 31;
        cp0[@enumToInt(Cop0Register.wired)] = 0;
        
        getStatusFlags().low_power_mode = false;
        getStatusFlags().diagnostic_field.bootstrap_exception_vector = true;
        getStatusFlags().diagnostic_field.tlb_shutdown = false;
        getStatusFlags().diagnostic_field.soft_reset_or_nmi = false;
        getStatusFlags().is_error = true;
        
        // According to PeterLemon's N64 test suite, these are all one. Which probably means 
        // on the N64 they've removed any kind of this functionality probably to reduce cost.
        getStatusFlags().is_64bit_kernel_address_space = true;
        getStatusFlags().is_64bit_supervisor_address_space = true;
        getStatusFlags().is_64bit_user_address_space = true;

        current_exception = .cold_reset;
    }

    pub fn raiseSoftResetOrNmi() void {
        if (system.config.hle_pif) {
            log.warn("Cannot raise a soft reset / NMI with an HLE PIF currently.");
            return;
        }
        if (current_exception != null and @enumToInt(ExceptionType.soft_reset_or_nmi) > @enumToInt(current_exception.?)) return;

        getStatusFlags().low_power_mode = false;
        getStatusFlags().diagnostic_field.flags.tlb_shutdown = false;
        getStatusFlags().diagnostic_field.bootstrap_exception_vector = true;
        getStatusFlags().diagnostic_field.flags.soft_reset_or_nmi = true;
        getStatusFlags().is_error = true;

        current_exception = .soft_reset_or_nmi;
    }

    pub fn raiseCommonException(t: ExceptionType) void {
        if (current_exception != null and @enumToInt(t) > @enumToInt(current_exception.?)) return;

        getCauseFlags().exc_code = typeToExcCode(t) catch {
            log.err("Cannot raise a common exception for a non-common exception type!", .{ });
            return;
        };

        current_exception = t;
    }

    // This function assumes that you've already set all the 
    // relevant registers describing the kind of exception and exception info.
    inline fn jumpToExceptionHandler() void {
        if (!getStatusFlags().is_error) {
            var vector_offset: u32 = 0x000;
            if (!getStatusFlags().is_exception) {
                getCauseFlags().branch_delay_slot = is_delay_slot;
                cp0[@enumToInt(Cop0Register.epc)] = switch(is_delay_slot) {
                    true => last_pc - 4,
                    false => last_pc
                };
            } else {
                vector_offset = 0x180;
            }
            getStatusFlags().is_exception = true;
            pc = switch (getStatusFlags().diagnostic_field.bootstrap_exception_vector) {
                true => 0xBFC00200 + vector_offset,
                false => 0x80000000 + vector_offset
            };
        } else {
            cp0[@enumToInt(Cop0Register.errorepc)] = last_pc;
            pc = 0xBFC00000;
            if (system.config.hle_pif) {
                runHlePif();
            }
        }
        overrideBranch();
    }
};

const interpreter = struct {
    const opcode_lookup = init: { 
        var table = [_]*const fn(Instruction) CpuError!void { instStub } ** (std.math.maxInt(u6)+1);
        table[0b001000] = instAddi;
        table[0b001001] = instAddiu;
        table[0b001100] = instAndi;
        table[0b010001] = instCop1;
        table[0b000100] = instBeq;
        table[0b010100] = instBeql;
        table[0b000001] = instRegimm;
        table[0b000111] = instBgtz;
        table[0b010111] = instBgtzl;
        table[0b000110] = instBlez;
        table[0b010110] = instBlezl;
        table[0b000101] = instBne;
        table[0b010101] = instBnel;
        table[0b101111] = instCache;
        table[0b011000] = instDaddi;
        table[0b011001] = instDaddiu;
        table[0b010000] = instCop0;
        table[0b000010] = instJ;
        table[0b000011] = instJal;
        table[0b100000] = instLb;
        table[0b100100] = instLbu;
        table[0b110111] = instLd;
        table[0b110101] = instLdc1;
        // TODO: Implement LDL and LDR
        table[0b100001] = instLh;
        table[0b100101] = instLhu;
        table[0b001111] = instLui;
        table[0b100011] = instLw;
        table[0b110001] = instLwc1;
        // TODO: Implement LWL and LWR
        table[0b100111] = instLwu;
        table[0b001101] = instOri;
        table[0b101000] = instSb;
        table[0b111111] = instSd;
        table[0b111101] = instSdc1;
        // TODO: Implement SDL and SDR
        table[0b101001] = instSh;
        table[0b001010] = instSlti;
        table[0b001011] = instSltiu;
        table[0b101011] = instSw;
        table[0b111001] = instSwc1;
        // TODO: Implement SWL and SWR
        // TODO: Implement the TLB alongside the TLBP, TLBR, TLBWI, and TLBWR instructions
        table[0b001110] = instXori;
        break :init table;
    };

    const special_table = init: {
        var table = [_]*const fn(Instruction) CpuError!void { instStubSpecial } ** (std.math.maxInt(u6)+1);
        table[@enumToInt(Instruction.SpecialFunction.add)] = instAdd;
        table[@enumToInt(Instruction.SpecialFunction.addu)] = instAddu;
        table[@enumToInt(Instruction.SpecialFunction._and)] = instAnd;
        table[@enumToInt(Instruction.SpecialFunction._break)] = instBreak;
        table[@enumToInt(Instruction.SpecialFunction.dadd)] = instDadd;
        table[@enumToInt(Instruction.SpecialFunction.daddu)] = instDaddu;
        table[@enumToInt(Instruction.SpecialFunction.ddiv)] = instDdiv;
        table[@enumToInt(Instruction.SpecialFunction.ddivu)] = instDdivu;
        table[@enumToInt(Instruction.SpecialFunction.div)] = instDiv;
        table[@enumToInt(Instruction.SpecialFunction.divu)] = instDivu;
        table[@enumToInt(Instruction.SpecialFunction.dmult)] = instDmult;
        table[@enumToInt(Instruction.SpecialFunction.dmultu)] = instDmultu;
        table[@enumToInt(Instruction.SpecialFunction.dsll)] = instDsll;
        table[@enumToInt(Instruction.SpecialFunction.dsllv)] = instDsllv;
        table[@enumToInt(Instruction.SpecialFunction.dsll32)] = instDsll32;
        table[@enumToInt(Instruction.SpecialFunction.dsra)] = instDsra;
        table[@enumToInt(Instruction.SpecialFunction.dsrav)] = instDsrav;
        table[@enumToInt(Instruction.SpecialFunction.dsra32)] = instDsra32;
        table[@enumToInt(Instruction.SpecialFunction.dsrl)] = instDsrl;
        table[@enumToInt(Instruction.SpecialFunction.dsrlv)] = instDsrlv;
        table[@enumToInt(Instruction.SpecialFunction.dsrl32)] = instDsrl32;
        table[@enumToInt(Instruction.SpecialFunction.dsub)] = instDsub;
        table[@enumToInt(Instruction.SpecialFunction.dsubu)] = instDsubu;
        table[@enumToInt(Instruction.SpecialFunction.jalr)] = instJalr;
        table[@enumToInt(Instruction.SpecialFunction.jr)] = instJr;
        table[@enumToInt(Instruction.SpecialFunction.mfhi)] = instMfhi;
        table[@enumToInt(Instruction.SpecialFunction.mflo)] = instMflo;
        table[@enumToInt(Instruction.SpecialFunction.mthi)] = instMthi;
        table[@enumToInt(Instruction.SpecialFunction.mtlo)] = instMtlo;
        table[@enumToInt(Instruction.SpecialFunction.mult)] = instMult;
        table[@enumToInt(Instruction.SpecialFunction.multu)] = instMultu;
        table[@enumToInt(Instruction.SpecialFunction.nor)] = instNor;
        table[@enumToInt(Instruction.SpecialFunction._or)] = instOr;
        table[@enumToInt(Instruction.SpecialFunction.sll)] = instSll;
        table[@enumToInt(Instruction.SpecialFunction.sllv)] = instSllv;
        table[@enumToInt(Instruction.SpecialFunction.slt)] = instSlt;
        table[@enumToInt(Instruction.SpecialFunction.sltu)] = instSltu;
        table[@enumToInt(Instruction.SpecialFunction.sra)] = instSra;
        table[@enumToInt(Instruction.SpecialFunction.srav)] = instSrav;
        table[@enumToInt(Instruction.SpecialFunction.srl)] = instSrl;
        table[@enumToInt(Instruction.SpecialFunction.srlv)] = instSrlv;
        table[@enumToInt(Instruction.SpecialFunction.sub)] = instSub;
        table[@enumToInt(Instruction.SpecialFunction.subu)] = instSubu;
        table[@enumToInt(Instruction.SpecialFunction.sync)] = instNop;
        table[@enumToInt(Instruction.SpecialFunction.xor)] = instXor;
        break :init table;
    };

    inline fn executeInstruction(inst: Instruction) CpuError!void {
        if (inst.opcode == 0b000000) {
            try special_table[@enumToInt(inst.data.r_type.function)](inst);
        } else {
            try opcode_lookup[inst.opcode](inst);
        }
    }

    inline fn branch(offset: i16) void {
        branch_target = @as(i32, offset) << 2;
    }

    inline fn jump(target: u26) void {
        jump_target = @as(u32, target) << 2;
    }

    inline fn jumpReg(reg: u5) void {
        jump_reg_target = @truncate(u32, gpr[reg]);
    }

    inline fn linkReg(reg: u5) void {
        gpr[reg] = pc + 8;
    }

    inline fn link() void {
        linkReg(31);
    }

    inline fn calculateAddress(base: u5, offset: i16) u32 {
        return @bitCast(u32, @bitCast(i32, @truncate(u32, gpr[base])) + @as(i32, offset));
    }

    fn instNop(_: Instruction) CpuError!void {
        pc += 4;
    }

    fn instStubSpecial(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        log.err("Unknown SPECIAL function 0b{b}", .{ @enumToInt(r_type.function) });
        return error.UnknownInstruction;
    }

    fn instAdd(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        var rs = @truncate(i32, @bitCast(i64, gpr[r_type.rs]));
        var rt = @truncate(i32, @bitCast(i64, gpr[r_type.rt]));
        var result = @addWithOverflow(rs, rt);
        if (result.@"1" == 1) {
            cop0.raiseCommonException(.overflow);
            pc += 4;
            return;
        }
        gpr[r_type.rd] = @bitCast(u64, @as(i64, result.@"0"));
        pc += 4;
    }

    fn instAddi(inst: Instruction) CpuError!void {
        const i_type = inst.data.i_type;
        var rs = @truncate(i32, @bitCast(i64, gpr[i_type.rs]));
        var immediate = @as(i32, @bitCast(i16, i_type.immediate));
        var result = @addWithOverflow(rs, immediate);
        if (result.@"1" == 1) {
            cop0.raiseCommonException(.overflow);
            pc += 4;
            return;
        }
        gpr[i_type.rt] = @bitCast(u64, @as(i64, result.@"0"));
        pc += 4;
    }

    fn instAddiu(inst: Instruction) CpuError!void {
        const i_type = inst.data.i_type;
        var rs = @truncate(i32, @bitCast(i64, gpr[i_type.rs]));
        var immediate = @as(i32, @bitCast(i16, i_type.immediate));
        gpr[i_type.rt] = @bitCast(u64, @as(i64, rs +% immediate));
        pc += 4;
    }

    fn instAddu(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        var rs = @truncate(i32, @bitCast(i64, gpr[r_type.rs]));
        var rt = @truncate(i32, @bitCast(i64, gpr[r_type.rt]));
        gpr[r_type.rd] = @bitCast(u64, @as(i64, rs +% rt));
        pc += 4;
    }

    fn instAnd(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = gpr[r_type.rs] & gpr[r_type.rt];
        pc += 4;
    }

    fn instAndi(inst: Instruction) CpuError!void {
        const i_type = inst.data.i_type;
        gpr[i_type.rt] = gpr[i_type.rs] & i_type.immediate;
        pc += 4;
    }

    fn instCop1(inst: Instruction) CpuError!void {
        const i_type_branch = inst.data.i_type_cop_branch;
        const r_type_cop = inst.data.r_type_cop;
        if (i_type_branch.sub_opcode == Instruction.CopSubOpcode.bc) {
            // TODO: Check if the Branch condition needs to be sampled *before* 
            // this instruction executes to insure correct behaviour.
            if (i_type_branch.condition == Instruction.CopBranchCondition.bcf) {
                try instBc1f(inst);
                return;
            } else if (i_type_branch.condition == Instruction.CopBranchCondition.bcfl) {
                try instBc1fl(inst);
                return;
            } else if (i_type_branch.condition == Instruction.CopBranchCondition.bct) {
                try instBc1t(inst);
                return;
            } else if (i_type_branch.condition == Instruction.CopBranchCondition.bctl) {
                try instBc1tl(inst);
                return;
            } else {
                log.err("Unknown COP branch condition 0b{b}", .{ @enumToInt(i_type_branch.condition) });
                return error.UnknownInstruction;
            }
        } else if (r_type_cop.sub_opcode == Instruction.CopSubOpcode.cf) {
            try instCfc1(inst);
            return;
        } else if (r_type_cop.sub_opcode == Instruction.CopSubOpcode.ct) {
            try instCtc1(inst);
            return;
        } else if (r_type_cop.sub_opcode == Instruction.CopSubOpcode.mf) {
            try instMfc1(inst);
            return;
        } else if (r_type_cop.sub_opcode == Instruction.CopSubOpcode.mt) {
            try instMtc1(inst);
            return;
        } else {
            log.err("Unknown COP1 sub-opcode 0b{b}", .{ @enumToInt(i_type_branch.sub_opcode) });
            return error.UnknownInstruction;
        }
    }

    inline fn instBc1f(inst: Instruction) CpuError!void {
        const i_type_branch = inst.data.i_type_cop_branch;
        if (!cp1_coc) {
            branch(i_type_branch.offset);
        }
        pc += 4;
    }

    inline fn instBc1fl(inst: Instruction) CpuError!void {
        const i_type_branch = inst.data.i_type_cop_branch;
        if (!cp1_coc) {
            branch(i_type_branch.offset);
            pc += 4;
        } else {
            pc += 8;
        }
    }

    inline fn instBc1t(inst: Instruction) CpuError!void {
        const i_type_branch = inst.data.i_type_cop_branch;
        if (cp1_coc) {
            branch(i_type_branch.offset);
        }
        pc += 4;
    }

    inline fn instBc1tl(inst: Instruction) CpuError!void {
        const i_type_branch = inst.data.i_type_cop_branch;
        if (cp1_coc) {
            branch(i_type_branch.offset);
            pc += 4;
        } else {
            pc += 8;
        }
    }

    fn instBeq(inst: Instruction) CpuError!void {
        const i_type_branch = inst.data.i_type_branch;
        if (gpr[i_type_branch.rs] == gpr[i_type_branch.rt]) {
            branch(i_type_branch.offset);
        }
        pc += 4;
    }

    fn instBeql(inst: Instruction) CpuError!void {
        const i_type_branch = inst.data.i_type_branch;
        if (gpr[i_type_branch.rs] == gpr[i_type_branch.rt]) {
            branch(i_type_branch.offset);
            pc += 4;
        } else {
            pc += 8;
        }
    }

    fn instRegimm(inst: Instruction) CpuError!void {
        const i_type_branch = inst.data.i_type_regimm_branch;
        if (i_type_branch.sub_opcode == Instruction.RegimmSubOpcode.bgez) {
            try instBgez(inst);
            return;
        } else if (i_type_branch.sub_opcode == Instruction.RegimmSubOpcode.bgezal) {
            try instBgezal(inst);
            return;
        } else if (i_type_branch.sub_opcode == Instruction.RegimmSubOpcode.bgezall) {
            try instBgezall(inst);
            return;
        } else if (i_type_branch.sub_opcode == Instruction.RegimmSubOpcode.bgezl) {
            try instBgezl(inst);
            return;
        } else if (i_type_branch.sub_opcode == Instruction.RegimmSubOpcode.bltz) {
            try instBltz(inst);
            return;
        } else if (i_type_branch.sub_opcode == Instruction.RegimmSubOpcode.bltzal) {
            try instBltzal(inst);
            return;
        } else if (i_type_branch.sub_opcode == Instruction.RegimmSubOpcode.bltzall) {
            try instBltzall(inst);
            return;
        } else if (i_type_branch.sub_opcode == Instruction.RegimmSubOpcode.bltzl) {
            try instBltzl(inst);
            return;
        } else {
            log.err("Unknown REGIMM sub-opcode 0b{b}", .{ @enumToInt(i_type_branch.sub_opcode) });
            return error.UnknownInstruction;
        }
    }

    inline fn instBgez(inst: Instruction) CpuError!void {
        const i_type_branch = inst.data.i_type_regimm_branch;
        var rs = @bitCast(i64, gpr[i_type_branch.rs]);
        if (rs >= 0) {
            branch(i_type_branch.offset);
        }
        pc += 4;
    }

    inline fn instBgezal(inst: Instruction) CpuError!void {
        const i_type_branch = inst.data.i_type_regimm_branch;
        var rs = @bitCast(i64, gpr[i_type_branch.rs]);
        link();
        if (rs >= 0) {
            branch(i_type_branch.offset);
        }
        pc += 4;
    }

    inline fn instBgezall(inst: Instruction) CpuError!void {
        const i_type_branch = inst.data.i_type_regimm_branch;
        var rs = @bitCast(i64, gpr[i_type_branch.rs]);
        link();
        if (rs >= 0) {
            branch(i_type_branch.offset);
            pc += 4;
        } else {
            pc += 8;
        }
    }

    inline fn instBgezl(inst: Instruction) CpuError!void {
        const i_type_branch = inst.data.i_type_regimm_branch;
        var rs = @bitCast(i64, gpr[i_type_branch.rs]);
        if (rs >= 0) {
            branch(i_type_branch.offset);
            pc += 4;
        } else {
            pc += 8;
        }
    }

    fn instBgtz(inst: Instruction) CpuError!void {
        const i_type_branch = inst.data.i_type_branch;
        var rs = @bitCast(i64, gpr[i_type_branch.rs]);
        if (rs > 0) {
            branch(i_type_branch.offset);
        }
        pc += 4;
    }

    fn instBgtzl(inst: Instruction) CpuError!void {
        const i_type_branch = inst.data.i_type_branch;
        var rs = @bitCast(i64, gpr[i_type_branch.rs]);
        if (rs > 0) {
            branch(i_type_branch.offset);
            pc += 4;
        } else {
            pc += 8;
        }
    }

    fn instBlez(inst: Instruction) CpuError!void {
        const i_type_branch = inst.data.i_type_branch;
        var rs = @bitCast(i64, gpr[i_type_branch.rs]);
        if (rs <= 0) {
            branch(i_type_branch.offset);
        }
        pc += 4;
    }

    fn instBlezl(inst: Instruction) CpuError!void {
        const i_type_branch = inst.data.i_type_branch;
        var rs = @bitCast(i64, gpr[i_type_branch.rs]);
        if (rs <= 0) {
            branch(i_type_branch.offset);
            pc += 4;
        } else {
            pc += 8;
        }
    }

    inline fn instBltz(inst: Instruction) CpuError!void {
        const i_type_branch = inst.data.i_type_regimm_branch;
        var rs = @bitCast(i64, gpr[i_type_branch.rs]);
        if (rs < 0) {
            branch(i_type_branch.offset);
        }
        pc += 4;
    }

    inline fn instBltzal(inst: Instruction) CpuError!void {
        const i_type_branch = inst.data.i_type_regimm_branch;
        var rs = @bitCast(i64, gpr[i_type_branch.rs]);
        link();
        if (rs < 0) {
            branch(i_type_branch.offset);
        }
        pc += 4;
    }

    inline fn instBltzall(inst: Instruction) CpuError!void {
        const i_type_branch = inst.data.i_type_regimm_branch;
        var rs = @bitCast(i64, gpr[i_type_branch.rs]);
        link();
        if (rs < 0) {
            branch(i_type_branch.offset);
            pc += 4;
        } else {
            pc += 8;
        }
    }

    inline fn instBltzl(inst: Instruction) CpuError!void {
        const i_type_branch = inst.data.i_type_regimm_branch;
        var rs = @bitCast(i64, gpr[i_type_branch.rs]);
        if (rs < 0) {
            branch(i_type_branch.offset);
            pc += 4;
        } else {
            pc += 8;
        }
    }

    fn instBne(inst: Instruction) CpuError!void {
        const i_type_branch = inst.data.i_type_branch;
        if (gpr[i_type_branch.rs] != gpr[i_type_branch.rt]) {
            branch(i_type_branch.offset);
        }
        pc += 4;
    }

    fn instBnel(inst: Instruction) CpuError!void {
        const i_type_branch = inst.data.i_type_branch;
        if (gpr[i_type_branch.rs] != gpr[i_type_branch.rt]) {
            branch(i_type_branch.offset);
            pc += 4;
        } else {
            pc += 8;
        }
    }

    fn instBreak(_: Instruction) CpuError!void {
        log.err("BREAK instruction not implemented.", .{ });
        return error.NotImplemented;
    }

    fn instCache(_: Instruction) CpuError!void {
        pc += 4;
    }

    inline fn instCfc1(inst: Instruction) CpuError!void {
        const r_type_cop = inst.data.r_type_cop;
        if (r_type_cop.rt == 0) {
            gpr[r_type_cop.rd] = @bitCast(u64, @as(i64, fcr0));
        } else if (r_type_cop.rt == 31) {
            gpr[r_type_cop.rd] = @bitCast(u64, @as(i64, fcr31));
        } else {
            log.err("The CFC1 instruction only works with the FCRs 0 and 31", .{ });
            return error.InvalidInstruction;
        }
        pc += 4;
    }

    inline fn instCtc1(inst: Instruction) CpuError!void {
        const r_type_cop = inst.data.r_type_cop;
        if (r_type_cop.rt == 0) {
            fcr0 = @truncate(u32, gpr[r_type_cop.rt]);
        } else if (r_type_cop.rt == 31) {
            fcr31 = @truncate(u32, gpr[r_type_cop.rt]);
        } else {
            log.err("The CTC1 instruction only works with the FCRs 0 and 31", .{ });
            return error.InvalidInstruction;
        }
        cp1_coc = getFcr31Flags().condition;
        pc += 4;
    }

    fn instDadd(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        var result = @addWithOverflow(@bitCast(i64, gpr[r_type.rs]), @bitCast(i64, gpr[r_type.rt]));
        if (result.@"1" == 1) {
            cop0.raiseCommonException(.overflow);
            pc += 4;
            return;
        }
        gpr[r_type.rd] = @bitCast(u64, result.@"0");
        pc += 4;
    }

    fn instDaddi(inst: Instruction) CpuError!void {
        const i_type = inst.data.i_type;
        var immediate = @as(i64, @bitCast(i16, i_type.immediate));
        var result = @addWithOverflow(@bitCast(i64, gpr[i_type.rs]), immediate);
        if (result.@"1" == 1) {
            cop0.raiseCommonException(.overflow);
            pc += 4;
            return;
        }
        gpr[i_type.rt] = @bitCast(u64, result.@"0");
        pc += 4;
    }

    fn instDaddiu(inst: Instruction) CpuError!void {
        const i_type = inst.data.i_type;
        var immediate = @as(i64, @bitCast(i16, i_type.immediate));
        gpr[i_type.rt] = @bitCast(u64, @bitCast(i64, gpr[i_type.rs]) +% immediate);
        pc += 4;
    }

    fn instDaddu(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = @bitCast(u64, @bitCast(i64, gpr[r_type.rs]) +% @bitCast(i64, gpr[r_type.rt]));
        pc += 4;
    }

    fn instDdiv(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        var numerator = @bitCast(i64, gpr[r_type.rs]);
        var denominator = @bitCast(i64, gpr[r_type.rt]);
        if (denominator == 0) {
            hi = @bitCast(u64, numerator);
            lo = @bitCast(u64, -std.math.sign(numerator));
            pc += 4;
            return;
        }
        lo = @bitCast(u64, @divTrunc(numerator, denominator));
        hi = @bitCast(u64, @mod(numerator, denominator));
        pc += 4;
    }

    fn instDdivu(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        var numerator = gpr[r_type.rs];
        var denominator = gpr[r_type.rt];
        if (denominator == 0) {
            hi = numerator;
            lo = @bitCast(u64, @as(i64, -1));
            pc += 4;
            return;
        }
        lo = @divTrunc(numerator, denominator);
        hi = @mod(numerator, denominator);
        pc += 4;
    }

    fn instDiv(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        var numerator = @truncate(i32, @bitCast(i64, gpr[r_type.rs]));
        var denominator = @truncate(i32, @bitCast(i64, gpr[r_type.rt]));
        if (denominator == 0) {
            hi = @bitCast(u64, @as(i64, numerator));
            lo = @bitCast(u64, @as(i64, -std.math.sign(numerator)));
            pc += 4;
            return;
        }
        lo = @bitCast(u64, @as(i64, @divTrunc(numerator, denominator)));
        hi = @bitCast(u64, @as(i64, @mod(numerator, denominator)));
        pc += 4;
    }

    fn instDivu(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        var numerator = @truncate(u32, gpr[r_type.rs]);
        var denominator = @truncate(u32, gpr[r_type.rt]);
        if (denominator == 0) {
            hi = @as(u64, numerator);
            lo = @bitCast(u64, @as(i64, -1));
            pc += 4;
            return;
        }
        lo = @as(u64, @divTrunc(numerator, denominator));
        hi = @as(u64, @mod(numerator, denominator));
        pc += 4;
    }

    fn instDmult(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        var result = @bitCast(u128, 
            @as(i128, @bitCast(i64, gpr[r_type.rs])) *% 
            @as(i128, @bitCast(i64, gpr[r_type.rt])));
        lo = @truncate(u64, result);
        hi = @truncate(u64, result >> 64);
        pc += 4;
    }

    fn instDmultu(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        var result = @as(u128, gpr[r_type.rs]) *% @as(u128, gpr[r_type.rt]);
        lo = @truncate(u64, result);
        hi = @truncate(u64, result >> 64);
        pc += 4;
    }

    fn instDsll(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = gpr[r_type.rt] << r_type.sa;
        pc += 4;
    }

    fn instDsllv(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = gpr[r_type.rt] << @truncate(u5, gpr[r_type.rs]);
        pc += 4;
    }

    fn instDsll32(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = gpr[r_type.rt] << (@as(u6, r_type.sa) + 32);
        pc += 4;
    }

    fn instDsra(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = @bitCast(u64, @bitCast(i64, gpr[r_type.rt]) >> r_type.sa);
        pc += 4;
    }

    fn instDsrav(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = @bitCast(u64, @bitCast(i64, gpr[r_type.rt]) >> @truncate(u5, gpr[r_type.rs]));
        pc += 4;
    }

    fn instDsra32(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = @bitCast(u64, @bitCast(i64, gpr[r_type.rt]) >> (@as(u6, r_type.sa) + 32));
        pc += 4;
    }

    fn instDsrl(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = gpr[r_type.rt] >> r_type.sa;
        pc += 4;
    }

    fn instDsrlv(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = gpr[r_type.rt] >> @truncate(u5, gpr[r_type.rs]);
        pc += 4;
    }

    fn instDsrl32(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = gpr[r_type.rt] >> (@as(u6, r_type.sa) + 32);
        pc += 4;
    }

    fn instDsub(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        var result = @subWithOverflow(@bitCast(i64, gpr[r_type.rs]), @bitCast(i64, gpr[r_type.rt]));
        if (result.@"1" == 1) {
            cop0.raiseCommonException(.overflow);
            pc += 4;
            return;
        }
        gpr[r_type.rd] = @bitCast(u64, result.@"0");
        pc += 4;
    }

    fn instDsubu(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = @bitCast(u64, @bitCast(i64, gpr[r_type.rs]) -% @bitCast(i64, gpr[r_type.rt]));
        pc += 4;
    }

    fn instCop0(inst: Instruction) CpuError!void {
        const generic = inst.data.cop0_generic;
        const r_type = inst.data.r_type_cop;
        if (generic.function == Instruction.Cop0Function.eret) {
            try instEret(inst);
            return;
        } else if (r_type.sub_opcode == Instruction.CopSubOpcode.mf) {
            try instMfc0(inst);
            return;
        } else if (r_type.sub_opcode == Instruction.CopSubOpcode.mt) {
            try instMtc0(inst);
            return;
        } else {
            log.err("Unknown COP0 function 0b{b}", .{ @enumToInt(generic.function) });
            return error.UnknownInstruction;
        }
    }

    inline fn instEret(_: Instruction) CpuError!void {
        const status = getStatusFlags();
        if (status.is_error) {
            pc = cp0[@enumToInt(Cop0Register.errorepc)];
            status.is_error = false;
        } else {
            pc = cp0[@enumToInt(Cop0Register.epc)];
            status.is_exception = false;
        }
        ll_bit = false;
    }

    fn instJ(inst: Instruction) CpuError!void {
        const j_type = inst.data.j_type;
        jump(j_type.target);
        pc += 4;
    }

    fn instJal(inst: Instruction) CpuError!void {
        const j_type = inst.data.j_type;
        link();
        jump(j_type.target);
        pc += 4;
    }

    fn instJalr(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        linkReg(r_type.rd);
        jumpReg(r_type.rs);
        pc += 4;
    }

    fn instJr(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        jumpReg(r_type.rs);
        pc += 4;
    }

    fn instLb(inst: Instruction) CpuError!void {
        const i_type = inst.data.i_type_mem;
        var physical_address = virtualToPhysical(calculateAddress(i_type.base, i_type.offset));
        var result = cpu_bus.readAligned(u8, physical_address);
        gpr[i_type.rt] = @bitCast(u64, @as(i64, @bitCast(i8, result)));
        pc += 4;
    }

    fn instLbu(inst: Instruction) CpuError!void {
        const i_type = inst.data.i_type_mem;
        var physical_address = virtualToPhysical(calculateAddress(i_type.base, i_type.offset));
        var result = cpu_bus.readAligned(u8, physical_address);
        gpr[i_type.rt] = result;
        pc += 4;
    }

    fn instLd(inst: Instruction) CpuError!void {
        const i_type = inst.data.i_type_mem;
        const address = calculateAddress(i_type.base, i_type.offset);
        if (address & 0b111 != 0) {
            cp0[@enumToInt(Cop0Register.bad_vaddr)] = address;
            cop0.raiseCommonException(.address_error_load);
            pc += 4;
            return;
        }
        const physical_address = virtualToPhysical(address);
        gpr[i_type.rt] = cpu_bus.readAligned(u64, physical_address);
        pc += 4;
    }

    fn instLdc1(inst: Instruction) CpuError!void {
        const i_type = inst.data.i_type_mem;
        const address = calculateAddress(i_type.base, i_type.offset);
        if (address & 0b111 != 0) {
            cp0[@enumToInt(Cop0Register.bad_vaddr)] = address;
            cop0.raiseCommonException(.address_error_load);
            pc += 4;
            return;
        }
        const physical_address = virtualToPhysical(address);
        // Note: Ignoring the FR bit in the Status Register for speed reasons, as it's not necessary.
        fpr[i_type.rt] = cpu_bus.readAligned(u64, physical_address);
        pc += 4;
    }

    // TODO: Implement LDL and LDR

    fn instLh(inst: Instruction) CpuError!void {
        const i_type = inst.data.i_type_mem;
        const address = calculateAddress(i_type.base, i_type.offset);
        if (address & 0b1 != 0) {
            cp0[@enumToInt(Cop0Register.bad_vaddr)] = address;
            cop0.raiseCommonException(.address_error_load);
            pc += 4;
            return;
        }
        const physical_address = virtualToPhysical(address);
        gpr[i_type.rt] = @bitCast(u64, @as(i64, @bitCast(i16, cpu_bus.readAligned(u16, physical_address))));
        pc += 4;
    }

    fn instLhu(inst: Instruction) CpuError!void {
        const i_type = inst.data.i_type_mem;
        const address = calculateAddress(i_type.base, i_type.offset);
        if (address & 0b1 != 0) {
            cp0[@enumToInt(Cop0Register.bad_vaddr)] = address;
            cop0.raiseCommonException(.address_error_load);
            pc += 4;
            return;
        }
        const physical_address = virtualToPhysical(address);
        gpr[i_type.rt] = cpu_bus.readAligned(u16, physical_address);
        pc += 4;
    }

    fn instLui(inst: Instruction) CpuError!void {
        const i_type = inst.data.i_type;
        gpr[i_type.rt] = @bitCast(u64, @as(i64, @as(i32, @bitCast(i16, i_type.immediate)) << 16));
        pc += 4;
    }

    fn instLw(inst: Instruction) CpuError!void {
        const i_type = inst.data.i_type_mem;
        const address = calculateAddress(i_type.base, i_type.offset);
        if (address & 0b11 != 0) {
            cp0[@enumToInt(Cop0Register.bad_vaddr)] = address;
            cop0.raiseCommonException(.address_error_load);
            pc += 4;
            return;
        }
        const physical_address = virtualToPhysical(address);
        gpr[i_type.rt] = @bitCast(u64, @as(i64, @bitCast(i32, cpu_bus.readAligned(u32, physical_address))));
        pc += 4;
    }

    fn instLwc1(inst: Instruction) CpuError!void {
        const i_type = inst.data.i_type_mem;
        const address = calculateAddress(i_type.base, i_type.offset);
        if (address & 0b11 != 0) {
            cp0[@enumToInt(Cop0Register.bad_vaddr)] = address;
            cop0.raiseCommonException(.address_error_load);
            pc += 4;
            return;
        }
        const physical_address = virtualToPhysical(address);
        // Note: Ignoring the FR bit in the Status Register for speed reasons, as it's not necessary.
        fpr[i_type.rt] = cpu_bus.readAligned(u32, physical_address);
        pc += 4;
    }

    // TODO: Implement LWL and LWR

    fn instLwu(inst: Instruction) CpuError!void {
        const i_type = inst.data.i_type_mem;
        const address = calculateAddress(i_type.base, i_type.offset);
        if (address & 0b11 != 0) {
            cp0[@enumToInt(Cop0Register.bad_vaddr)] = address;
            cop0.raiseCommonException(.address_error_load);
            pc += 4;
            return;
        }
        const physical_address = virtualToPhysical(address);
        gpr[i_type.rt] = cpu_bus.readAligned(u32, physical_address);
        pc += 4;
    }

    inline fn instMfc0(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type_cop;
        gpr[r_type.rt] = cp0[r_type.rd];
        pc += 4;
    }

    inline fn instMfc1(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type_cop;
        gpr[r_type.rt] = @bitCast(u64, @as(i64, @bitCast(i32, @truncate(u32, fpr[r_type.rd]))));
        pc += 4;
    }

    fn instMfhi(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = hi;
        pc += 4;
    }

    fn instMflo(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = lo;
        pc += 4;
    }
    
    inline fn instMtc0(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type_cop;
        if (r_type.rd == @enumToInt(Cop0Register.context)) {
            cp0[r_type.rd] = (@truncate(u32, gpr[r_type.rt]) & cp0_write_mask[r_type.rd]) 
                | (cp0[r_type.rd] & 0x007FFFF0);
        } else if (r_type.rd == @enumToInt(Cop0Register.wired)) {
            cp0[r_type.rd] = @truncate(u32, gpr[r_type.rt]);
            cp0[@enumToInt(Cop0Register.random)] = 31;
        } else if (r_type.rd == @enumToInt(Cop0Register.compare)) {
            getCauseFlags().interrupts_pending.flags.timer_interrupt = false;
            cp0[r_type.rd] = @truncate(u32, gpr[r_type.rt]);
        } else if (r_type.rd == @enumToInt(Cop0Register.cause)) {
            cp0[r_type.rd] &= ~cp0_write_mask[r_type.rd];
            cp0[r_type.rd] |= @truncate(u32, gpr[r_type.rt]) & cp0_write_mask[r_type.rd];
        } else if (r_type.rd == @enumToInt(Cop0Register.taghi)) {
            cp0[r_type.rd] = 0;
        } else {
            if (cp0_write_mask[r_type.rd] == 0) {
                pc += 4;
                return;
            }
            cp0[r_type.rd] = @truncate(u32, gpr[r_type.rt]) & cp0_write_mask[r_type.rd];
        }
        pc += 4;
    }

    inline fn instMtc1(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type_cop;
        fpr[r_type.rd] = @truncate(u32, gpr[r_type.rt]);
        pc += 4;
    }

    fn instMthi(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        hi = gpr[r_type.rs];
        pc += 4;
    }

    fn instMtlo(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        lo = gpr[r_type.rs];
        pc += 4;
    }

    fn instMult(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        var result =
            @as(i64, @bitCast(i32, @truncate(u32, gpr[r_type.rs]))) *% 
            @as(i64, @bitCast(i32, @truncate(u32, gpr[r_type.rt])));
        lo = @bitCast(u64, @as(i64, @truncate(i32, result)));
        hi = @bitCast(u64, @as(i64, @truncate(i32, result >> 32)));
        pc += 4;
    }

    fn instMultu(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        var result = @as(u64, @truncate(u32, gpr[r_type.rs])) *% @as(u64, @truncate(u32, gpr[r_type.rt]));
        lo = @bitCast(u64, @as(i64, @bitCast(i32, @truncate(u32, result))));
        hi = @bitCast(u64, @as(i64, @bitCast(i32, @truncate(u32, result >> 32))));
        pc += 4;
    }

    fn instNor(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = ~(gpr[r_type.rs] | gpr[r_type.rt]);
        pc += 4;
    }

    fn instOr(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = gpr[r_type.rs] | gpr[r_type.rt];
        pc += 4;
    }

    fn instOri(inst: Instruction) CpuError!void {
        const i_type = inst.data.i_type;
        gpr[i_type.rt] = gpr[i_type.rs] | @as(u64, i_type.immediate);
        pc += 4;
    }

    fn instSb(inst: Instruction) CpuError!void {
        const i_type = inst.data.i_type_mem;
        var physical_address = virtualToPhysical(calculateAddress(i_type.base, i_type.offset));
        cpu_bus.writeAligned(physical_address, @truncate(u8, gpr[i_type.rt]));
        pc += 4;
    }

    fn instSd(inst: Instruction) CpuError!void {
        const i_type = inst.data.i_type_mem;
        const address = calculateAddress(i_type.base, i_type.offset);
        if (address & 0b111 != 0) {
            cp0[@enumToInt(Cop0Register.bad_vaddr)] = address;
            cop0.raiseCommonException(.address_error_store);
            pc += 4;
            return;
        }
        const physical_address = virtualToPhysical(address);
        cpu_bus.writeAligned(physical_address, gpr[i_type.rt]);
        pc += 4;
    }

    fn instSdc1(inst: Instruction) CpuError!void {
        const i_type = inst.data.i_type_mem;
        const address = calculateAddress(i_type.base, i_type.offset);
        if (address & 0b111 != 0) {
            cp0[@enumToInt(Cop0Register.bad_vaddr)] = address;
            cop0.raiseCommonException(.address_error_store);
            pc += 4;
            return;
        }
        const physical_address = virtualToPhysical(address);
        // Note: Ignoring the FR bit in the Status Register for speed reasons, as it's not necessary.
        cpu_bus.writeAligned(physical_address, fpr[i_type.rt]);
        pc += 4;
    }

    // TODO: Implement SDL and SDR

    fn instSh(inst: Instruction) CpuError!void {
        const i_type = inst.data.i_type_mem;
        const address = calculateAddress(i_type.base, i_type.offset);
        if (address & 0b1 != 0) {
            cp0[@enumToInt(Cop0Register.bad_vaddr)] = address;
            cop0.raiseCommonException(.address_error_store);
            pc += 4;
            return;
        }
        const physical_address = virtualToPhysical(address);
        cpu_bus.writeAligned(physical_address, @truncate(u16, gpr[i_type.rt]));
        pc += 4;
    }

    fn instSll(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = @bitCast(u64, @as(i64, 
            @bitCast(i32, @truncate(u32, gpr[r_type.rt]) << r_type.sa)));
        pc += 4;
    }

    fn instSllv(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = @bitCast(u64, @as(i64, 
            @bitCast(i32, @truncate(u32, gpr[r_type.rt]) << @truncate(u5, gpr[r_type.rs]))));
        pc += 4;
    }

    fn instSlt(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = @boolToInt(@bitCast(i64, gpr[r_type.rs]) < @bitCast(i64, gpr[r_type.rt]));
        pc += 4;
    }

    fn instSlti(inst: Instruction) CpuError!void {
        const i_type = inst.data.i_type;
        gpr[i_type.rt] = @boolToInt(@bitCast(i64, gpr[i_type.rs]) < @bitCast(i16, i_type.immediate));
        pc += 4;
    }

    fn instSltiu(inst: Instruction) CpuError!void {
        const i_type = inst.data.i_type;
        gpr[i_type.rt] = @boolToInt(gpr[i_type.rs] < @bitCast(i16, i_type.immediate));
        pc += 4;
    }

    fn instSltu(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = @boolToInt(gpr[r_type.rs] < gpr[r_type.rt]);
        pc += 4;
    }

    fn instSra(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = @bitCast(u64, @as(i64, 
            @bitCast(i32, @truncate(u32, gpr[r_type.rt])) >> r_type.sa));
        pc += 4;
    }

    fn instSrav(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = @bitCast(u64, @as(i64, 
            @bitCast(i32, @truncate(u32, gpr[r_type.rt])) >> @truncate(u5, gpr[r_type.rs])));
        pc += 4;
    }

    fn instSrl(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = @bitCast(u64, @as(i64, @bitCast(i32,
            @truncate(u32, gpr[r_type.rt]) >> r_type.sa)));
        pc += 4;
    }

    fn instSrlv(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = @bitCast(u64, @as(i64, @bitCast(i32, 
            @truncate(u32, gpr[r_type.rt]) >> @truncate(u5, gpr[r_type.rs]))));
        pc += 4;
    }

    fn instSub(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        var rs = @truncate(i32, @bitCast(i64, gpr[r_type.rs]));
        var rt = @truncate(i32, @bitCast(i64, gpr[r_type.rt]));
        var result = @subWithOverflow(rs, rt);
        if (result.@"1" == 1) {
            cop0.raiseCommonException(.overflow);
            pc += 4;
            return;
        }
        gpr[r_type.rd] = @bitCast(u64, @as(i64, result.@"0"));
        pc += 4;
    }

    fn instSubu(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        var rs = @truncate(i32, @bitCast(i64, gpr[r_type.rs]));
        var rt = @truncate(i32, @bitCast(i64, gpr[r_type.rt]));
        gpr[r_type.rd] = @bitCast(u64, @as(i64, rs -% rt));
        pc += 4;
    }

    fn instSw(inst: Instruction) CpuError!void {
        const i_type = inst.data.i_type_mem;
        const address = calculateAddress(i_type.base, i_type.offset);
        if (address & 0b11 != 0) {
            cp0[@enumToInt(Cop0Register.bad_vaddr)] = address;
            cop0.raiseCommonException(.address_error_store);
            pc += 4;
            return;
        }
        const physical_address = virtualToPhysical(address);
        cpu_bus.writeAligned(physical_address, @truncate(u32, gpr[i_type.rt]));
        pc += 4;
    }

    fn instSwc1(inst: Instruction) CpuError!void {
        const i_type = inst.data.i_type_mem;
        const address = calculateAddress(i_type.base, i_type.offset);
        if (address & 0b11 != 0) {
            cp0[@enumToInt(Cop0Register.bad_vaddr)] = address;
            cop0.raiseCommonException(.address_error_store);
            pc += 4;
            return;
        }
        const physical_address = virtualToPhysical(address);
        // Note: Ignoring the FR bit in the Status Register for speed reasons, as it's not necessary.
        cpu_bus.writeAligned(physical_address, @truncate(u32, fpr[i_type.rt]));
        pc += 4;
    }

    // TODO: Implement SWL and SWR

    // TODO: Implement the TLB alongside the TLBP, TLBR, TLBWI, and TLBWR instructions

    fn instXor(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = gpr[r_type.rs] ^ gpr[r_type.rt];
        pc += 4;
    }

    fn instXori(inst: Instruction) CpuError!void {
        const i_type = inst.data.i_type;
        gpr[i_type.rt] = gpr[i_type.rs] ^ @as(u64, i_type.immediate);
        pc += 4;
    }

    fn instStub(inst: Instruction) CpuError!void {
        log.warn("Unimplemented Instruction 0b{b} at address 0x{X}!", .{ inst.opcode, pc });
        return error.NotImplemented;
    }
};
