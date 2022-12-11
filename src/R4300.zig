const std = @import("std");
const CpuIoMap = @import("CpuIoMap.zig");

pub var gpr = [_]u64{0} ** 32;

pub var fpr = [_]u64{0} ** 32;

pub var fcr0: u32 = 0;
pub var fcr31: u32 = 0;

pub const FloatingPointRoundingMode = enum(u2) {
    nearest = 0b00,
    toward_zero = 0b01,
    ceiling = 0b10,
    floor = 0b11
};

pub const FloatingPointControlRegister31 = packed struct {
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
const Cop0Register = enum(u8) {
    index,
    random,
    entrylo0,
    entrylo1,
    context,
    pagemask,
    wired,
    bad_vaddr = 8,
    count,
    entryhi,
    compare,
    status,
    cause,
    epc,
    previd,
    config,
    lladdr,
    watchlo,
    watchhi,
    xcontext,
    taglo = 28,
    taghi,
    errorepc
};

pub const InterruptBits = packed struct {
    software_interrupt0: bool,
    software_interrupt1: bool,
    external: packed union {
        flags: packed struct {
            // TODO
        },
        bits: u5
    },
    timer_interrupt: bool
};

pub const Cop0StatusRegister = packed struct {
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
    diagnostic_field: u9,

    reverse_endian_user: bool,
    floating_point_registers_enable: bool,
    low_power_mode: bool,

    coprocessor_enable: packed union {
        flags: packed struct {
            cop0_enable: bool,
            cop1_enable: bool,
            cop2_enable: bool,
            cop3_enable: bool
        },
        bits: u4
    }
};

pub fn getStatusFlags() *Cop0StatusRegister {
    return @ptrCast(*Cop0StatusRegister, &cp0[@enumToInt(Cop0Register.status)]);
}

pub const InstructionBits = packed union {
    instruction: Instruction,
    bits: u32
};

pub const Instruction = packed struct {
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
        jr     = 0b001000
    };

    pub const CopSubOpcode = enum(u5) {
        bc = 0b01000, // Branch Conditional
        cf = 0b00010, // Move Control From
        ct = 0b00110 // Move Control To
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
        cop0: packed struct {
            function: Cop0Function,
            _unused: u20
        }
    },
    opcode: u6
};

// On the N64 the PC is really only 32-bit, even though the processor is technically capable of a 64-bit mode.
pub var pc: u32 = 0;
var branch_target: ?i32 = null;
var jump_target: ?u32 = null;

pub const CpuError = error {
    UnknownInstruction,
    NotImplemented,
    InvalidInstruction
};

pub fn init(hle_pif: bool) void {
    pc = CpuIoMap.pif_boot_rom_addr;

    var t = InstructionBits { .bits = 0 };
    interpreter.opcode_lookup[0b001000](t.instruction) catch unreachable; // FOR TESTING, SO THAT ZIG WILL COMPILE ALL INSTRUCTIONS

    if (hle_pif) {
        @panic("PIF HLE not implemented!");
    }
}

pub fn step() CpuError!void {
    gpr[0] = 0x0000000000000000;
}

const interpreter = struct {
    pub const opcode_lookup = init: { 
        var table = [_]*const fn(Instruction) CpuError!void { instStub } ** std.math.maxInt(u6);
        table[0b000000] = instSpecial;
        table[0b001000] = instAddi;
        table[0b001001] = instAddiu;
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
        break :init table;
    };

    inline fn branch(offset: i16) void {
        branch_target = @as(i32, offset) << 2;
    }

    inline fn jump(target: u26) void {
        jump_target = @as(u32, target) << 2;
    }

    inline fn jump_reg(reg: u5) void {
        jump_target = @truncate(u32, gpr[reg]);
    }

    inline fn link_reg(reg: u5) void {
        gpr[reg] = pc + 8;
    }

    inline fn link() void {
        link_reg(31);
    }

    fn instSpecial(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        if (r_type.function == Instruction.SpecialFunction.add) {
            try instAdd(inst);
            return;
        } else if (r_type.function == Instruction.SpecialFunction.addu) {
            try instAddu(inst);
            return;
        } else if(r_type.function == Instruction.SpecialFunction._and) {
            try instAnd(inst);
            return;
        } else if (r_type.function == Instruction.SpecialFunction._break) {
            try instBreak(inst);
            return;
        } else if (r_type.function == Instruction.SpecialFunction.dadd) {
            try instDadd(inst);
            return;
        } else if (r_type.function == Instruction.SpecialFunction.daddu) {
            try instDaddu(inst);
            return;
        } else if (r_type.function == Instruction.SpecialFunction.ddiv) {
            try instDdiv(inst);
            return;
        } else if (r_type.function == Instruction.SpecialFunction.ddivu) {
            try instDdivu(inst);
            return;
        } else if (r_type.function == Instruction.SpecialFunction.div) {
            try instDiv(inst);
            return;
        } else if (r_type.function == Instruction.SpecialFunction.divu) {
            try instDivu(inst);
            return;
        } else if (r_type.function == Instruction.SpecialFunction.dmult) {
            try instDmult(inst);
            return;
        } else if (r_type.function == Instruction.SpecialFunction.dmultu) {
            try instDmultu(inst);
            return;
        } else if (r_type.function == Instruction.SpecialFunction.dsll) {
            try instDsll(inst);
            return;
        } else if (r_type.function == Instruction.SpecialFunction.dsllv) {
            try instDsllv(inst);
            return;
        } else if (r_type.function == Instruction.SpecialFunction.dsll32) {
            try instDsll32(inst);
            return;
        } else if (r_type.function == Instruction.SpecialFunction.dsra) {
            try instDsra(inst);
            return;
        } else if (r_type.function == Instruction.SpecialFunction.dsrav) {
            try instDsrav(inst);
            return;
        } else if (r_type.function == Instruction.SpecialFunction.dsra32) {
            try instDsra32(inst);
            return;
        } else if (r_type.function == Instruction.SpecialFunction.dsrl) {
            try instDsrl(inst);
            return;
        } else if (r_type.function == Instruction.SpecialFunction.dsrlv) {
            try instDsrlv(inst);
            return;
        } else if (r_type.function == Instruction.SpecialFunction.dsrl32) {
            try instDsrl32(inst);
            return;
        } else if (r_type.function == Instruction.SpecialFunction.dsub) {
            try instDsub(inst);
            return;
        } else if (r_type.function == Instruction.SpecialFunction.dsubu) {
            try instDsubu(inst);
            return;
        } else if (r_type.function == Instruction.SpecialFunction.jalr) {
            try instJalr(inst);
            return;
        } else if (r_type.function == Instruction.SpecialFunction.jr) {
            try instJr(inst);
            return;
        } else {
            std.log.err("Unknown SPECIAL function {b}", .{ @enumToInt(r_type.function) });
            return error.UnknownInstruction;
        }
    }

    inline fn instAdd(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        var rs = @truncate(i32, @bitCast(i64, gpr[r_type.rs]));
        var rt = @truncate(i32, @bitCast(i64, gpr[r_type.rt]));
        var result: i32 = 0;
        if (@addWithOverflow(i32, rs, rt, &result)) {
            std.log.warn("TODO: Throw Integer Overflow exception in the R4300 CPU.", .{ });
            pc += 4;
            return;
        }
        gpr[r_type.rd] = @bitCast(u64, @as(i64, result));
        pc += 4;
    }

    fn instAddi(inst: Instruction) CpuError!void {
        const i_type = inst.data.i_type;
        var rs = @truncate(i32, @bitCast(i64, gpr[i_type.rs]));
        var immediate = @as(i32, @bitCast(i16, i_type.immediate));
        var result: i32 = 0;
        if (@addWithOverflow(i32, rs, immediate, &result)) {
            std.log.warn("TODO: Throw Integer Overflow exception in the R4300 CPU.", .{ });
            pc += 4;
            return;
        }
        gpr[i_type.rt] = @bitCast(u64, @as(i64, result));
        pc += 4;
    }

    fn instAddiu(inst: Instruction) CpuError!void {
        const i_type = inst.data.i_type;
        var rs = @truncate(i32, @bitCast(i64, gpr[i_type.rs]));
        var immediate = @as(i32, @bitCast(i16, i_type.immediate));
        gpr[i_type.rt] = @bitCast(u64, @as(i64, rs +% immediate));
        pc += 4;
    }

    inline fn instAddu(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        var rs = @truncate(i32, @bitCast(i64, gpr[r_type.rs]));
        var rt = @truncate(i32, @bitCast(i64, gpr[r_type.rt]));
        gpr[r_type.rd] = @bitCast(u64, @as(i64, rs +% rt));
        pc += 4;
    }

    inline fn instAnd(inst: Instruction) CpuError!void {
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
                std.log.err("Unknown COP branch condition {b}", .{ @enumToInt(i_type_branch.condition) });
                return error.UnknownInstruction;
            }
        } else if (r_type_cop.sub_opcode == Instruction.CopSubOpcode.cf) {
            try instCfc1(inst);
            return;
        } else if (r_type_cop.sub_opcode == Instruction.CopSubOpcode.ct) {
            try instCtc1(inst);
            return;
        } else {
            std.log.err("Unknown COP1 sub-opcode {b}", .{ @enumToInt(i_type_branch.sub_opcode) });
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
            std.log.err("Unknown REGIMM sub-opcode {b}", .{ @enumToInt(i_type_branch.sub_opcode) });
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

    inline fn instBreak(_: Instruction) CpuError!void {
        std.log.err("BREAK instruction not implemented.", .{ });
        return error.NotImplemented;
    }

    fn instCache(_: Instruction) CpuError!void {
        std.log.debug("CACHE Instruction Stubbed.", .{ });
        pc += 4;
    }

    inline fn instCfc1(inst: Instruction) CpuError!void {
        const r_type_cop = inst.data.r_type_cop;
        if (r_type_cop.rt == 0) {
            gpr[r_type_cop.rd] = @bitCast(u64, @as(i64, fcr0));
        } else if (r_type_cop.rt == 31) {
            gpr[r_type_cop.rd] = @bitCast(u64, @as(i64, fcr31));
        } else {
            std.log.err("The CFC1 instruction only works with the FCRs 0 and 31", .{ });
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
            std.log.err("The CTC1 instruction only works with the FCRs 0 and 31", .{ });
            return error.InvalidInstruction;
        }
        cp1_coc = getFcr31Flags().condition;
        pc += 4;
    }

    inline fn instDadd(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        var result: i64 = 0;
        if (@addWithOverflow(i64, @bitCast(i64, gpr[r_type.rs]), @bitCast(i64, gpr[r_type.rt]), &result)) {
            std.log.warn("TODO: Throw Integer Overflow exception in the R4300 CPU.", .{ });
            pc += 4;
            return;
        }
        gpr[r_type.rd] = @bitCast(u64, result);
        pc += 4;
    }

    fn instDaddi(inst: Instruction) CpuError!void {
        const i_type = inst.data.i_type;
        var immediate = @as(i64, @bitCast(i16, i_type.immediate));
        var result: i64 = 0;
        if (@addWithOverflow(i64, @bitCast(i64, gpr[i_type.rs]), immediate, &result)) {
            std.log.warn("TODO: Throw Integer Overflow exception in the R4300 CPU.", .{ });
            pc += 4;
            return;
        }
        gpr[i_type.rt] = @bitCast(u64, result);
        pc += 4;
    }

    fn instDaddiu(inst: Instruction) CpuError!void {
        const i_type = inst.data.i_type;
        var immediate = @as(i64, @bitCast(i16, i_type.immediate));
        gpr[i_type.rt] = @bitCast(u64, @bitCast(i64, gpr[i_type.rs]) +% immediate);
        pc += 4;
    }

    inline fn instDaddu(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = @bitCast(u64, @bitCast(i64, gpr[r_type.rs]) +% @bitCast(i64, gpr[r_type.rt]));
        pc += 4;
    }

    inline fn instDdiv(inst: Instruction) CpuError!void {
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

    inline fn instDdivu(inst: Instruction) CpuError!void {
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

    inline fn instDiv(inst: Instruction) CpuError!void {
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

    inline fn instDivu(inst: Instruction) CpuError!void {
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

    inline fn instDmult(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        var result = @bitCast(u128, 
            @as(i128, @bitCast(i64, gpr[r_type.rs])) *% 
            @as(i128, @bitCast(i64, gpr[r_type.rt])));
        lo = @truncate(u64, result);
        hi = @truncate(u64, result >> 64);
        pc += 4;
    }

    inline fn instDmultu(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        var result = @as(u128, gpr[r_type.rs]) *% @as(u128, gpr[r_type.rt]);
        lo = @truncate(u64, result);
        hi = @truncate(u64, result >> 64);
        pc += 4;
    }

    inline fn instDsll(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = gpr[r_type.rt] << r_type.sa;
        pc += 4;
    }

    inline fn instDsllv(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = gpr[r_type.rt] << @truncate(u5, gpr[r_type.rs]);
        pc += 4;
    }

    inline fn instDsll32(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = gpr[r_type.rt] << (@as(u6, r_type.sa) + 32);
        pc += 4;
    }

    inline fn instDsra(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = @bitCast(u64, @bitCast(i64, gpr[r_type.rt]) >> r_type.sa);
        pc += 4;
    }

    inline fn instDsrav(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = @bitCast(u64, @bitCast(i64, gpr[r_type.rt]) >> @truncate(u5, gpr[r_type.rs]));
        pc += 4;
    }

    inline fn instDsra32(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = @bitCast(u64, @bitCast(i64, gpr[r_type.rt]) >> (@as(u6, r_type.sa) + 32));
        pc += 4;
    }

    inline fn instDsrl(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = gpr[r_type.rt] >> r_type.sa;
        pc += 4;
    }

    inline fn instDsrlv(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = gpr[r_type.rt] >> @truncate(u5, gpr[r_type.rs]);
        pc += 4;
    }

    inline fn instDsrl32(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = gpr[r_type.rt] >> (@as(u6, r_type.sa) + 32);
        pc += 4;
    }

    inline fn instDsub(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        var result: i64 = 0;
        if (@subWithOverflow(i64, @bitCast(i64, gpr[r_type.rs]), @bitCast(i64, gpr[r_type.rt]), &result)) {
            std.log.warn("TODO: Throw Integer Overflow exception in the R4300 CPU.", .{ });
            pc += 4;
            return;
        }
        gpr[r_type.rd] = @bitCast(u64, result);
        pc += 4;
    }

    inline fn instDsubu(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = @bitCast(u64, @bitCast(i64, gpr[r_type.rs]) -% @bitCast(i64, gpr[r_type.rt]));
        pc += 4;
    }

    fn instCop0(inst: Instruction) CpuError!void {
        const cop = inst.data.cop0;
        if (cop.function == Instruction.Cop0Function.eret) {
            try instEret(inst);
            return;
        } else {
            std.log.err("Unknown COP0 function {b}", .{ @enumToInt(cop.function) });
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
    }

    fn instJal(inst: Instruction) CpuError!void {
        const j_type = inst.data.j_type;
        link();
        jump(j_type.target);
    }

    inline fn instJalr(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        link_reg(r_type.rd);
        jump_reg(r_type.rs);
    }

    inline fn instJr(inst: Instruction) CpuError!void {
        const r_type = inst.data.r_type;
        jump_reg(r_type.rs);
    }

    fn instNop(_: Instruction) CpuError!void {
        pc += 4;
    }

    fn instStub(inst: Instruction) CpuError!void {
        std.log.warn("Unimplemented Instruction 0b{b}!", .{ inst.opcode });
        return error.NotImplemented;
    }
};
