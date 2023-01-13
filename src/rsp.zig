const std = @import("std");
const memory = @import("memory.zig");
const system = @import("system.zig");

const log = std.log.scoped(.rsp);

pub var gpr = [_]u32{0} ** 32;

pub const Cop0Register = enum(u8) {
    sp_mem_addr,
    sp_dram_addr,
    sp_rd_len,
    sp_wr_len,
    sp_status,
    sp_dma_full,
    sp_dma_busy,
    sp_semaphore,
    dpc_start,
    dpc_end,
    dpc_current,
    dpc_status,
    dpc_clock,
    dpc_busy,
    dpc_pipe_busy,
    dpc_tmem_busy
};

pub const Instruction = packed struct(u32) {
    pub const SpecialFunction = enum(u6) {
        add    = 0b100000,
        addu   = 0b100001,
        _and   = 0b100100,
        _break = 0b001101,
        jalr   = 0b001001,
        jr     = 0b001000,
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
        cf = 0b00010, // Move Control From
        ct = 0b00110, // Move Control To
        mf = 0b00000, // Move From
        mt = 0b00100  // Move To
    };

    pub const RegimmSubOpcode = enum(u5) {
        bgez    = 0b00001,
        bgezal  = 0b10001,
        bltz    = 0b00000,
        bltzal  = 0b10000,
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
        }
    },
    opcode: u6
};

var branch_target: ?i32 = null;
var jump_target: ?u32 = null;
var jump_reg_target: ?u32 = null;
var is_delay_slot: bool = false;

pub const RspError = error {
    UnknownInstruction,
    NotImplemented,
    InvalidInstruction
};

pub fn getPc() *u32 {
    return memory.rcp.rsp.reg_range_1.getWordPtr(
        @enumToInt(memory.rcp.rsp.RegRange1Offset.sp_pc_reg));
}

pub fn init() void {
    memory.rcp.rsp.getSpStatusFlags().halt = true;
}

pub fn step() !void {
    if (memory.rcp.rsp.getSpStatusFlags().halt) {
        return;
    }

    gpr[0] = 0;

    var instruction_ptr = memory.rcp.rsp.imem.getWordPtr(getPc().*);
    try interpreter.executeInstruction(@ptrCast(*Instruction, instruction_ptr).*);
    getPc().* &= 0xFFF;

    // Handle jumps and branches.
    if ((jump_target != null or branch_target != null or jump_reg_target != null) and !is_delay_slot) {
        is_delay_slot = true;
    } else if (jump_target != null and is_delay_slot) {
        getPc().* = (getPc().* & 0xF0000000) | jump_target.?;
        jump_target = null;
        is_delay_slot = false;
    } else if (jump_reg_target != null and is_delay_slot) {
        getPc().* = jump_reg_target.?;
        jump_reg_target = null;
        is_delay_slot = false;
    } else if (branch_target != null and is_delay_slot) {
        getPc().* = (getPc().* - 4) +% @bitCast(u32, branch_target.?); // Relative to delay slot instruction.
        branch_target = null;
        is_delay_slot = false;
    }
}

const interpreter = struct {
    const opcode_lookup = init: { 
        var table = [_]*const fn(Instruction) RspError!void { instStub } ** (std.math.maxInt(u6)+1);
        table[0b001000] = instAddi;
        table[0b001001] = instAddi;
        table[0b001100] = instAndi;
        table[0b000100] = instBeq;
        table[0b000001] = instRegimm;
        table[0b000111] = instBgtz;
        table[0b000110] = instBlez;
        table[0b000101] = instBne;
        table[0b010000] = instCop0;
        table[0b000010] = instJ;
        table[0b000011] = instJal;
        table[0b100000] = instLb;
        table[0b100100] = instLbu;
        table[0b100001] = instLh;
        table[0b100101] = instLhu;
        table[0b001111] = instLui;
        table[0b100011] = instLw;
        table[0b001101] = instOri;
        table[0b101000] = instSb;
        table[0b101001] = instSh;
        table[0b001010] = instSlti;
        table[0b001011] = instSltiu;
        table[0b101011] = instSw;
        table[0b001110] = instXori;
        break :init table;
    };

    const special_table = init: {
        var table = [_]*const fn(Instruction) RspError!void { instStubSpecial } ** (std.math.maxInt(u6)+1);
        table[@enumToInt(Instruction.SpecialFunction.add)] = instAdd;
        table[@enumToInt(Instruction.SpecialFunction.addu)] = instAdd;
        table[@enumToInt(Instruction.SpecialFunction._and)] = instAnd;
        table[@enumToInt(Instruction.SpecialFunction._break)] = instBreak;
        table[@enumToInt(Instruction.SpecialFunction.jalr)] = instJalr;
        table[@enumToInt(Instruction.SpecialFunction.jr)] = instJr;
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
        table[@enumToInt(Instruction.SpecialFunction.subu)] = instSub;
        table[@enumToInt(Instruction.SpecialFunction.xor)] = instXor;
        break :init table;
    };

    inline fn executeInstruction(inst: Instruction) RspError!void {
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
        gpr[reg] = getPc().* +% 8;
    }

    inline fn link() void {
        linkReg(31);
    }

    inline fn calculateAddress(base: u5, offset: i16) u12 {
        return @truncate(u12, @bitCast(u32, @bitCast(i32, @truncate(u32, gpr[base])) + @as(i32, offset)));
    }

    fn instStub(inst: Instruction) RspError!void {
        log.err("Unimplemented Instruction 0b{b} at address 0x{X}!", .{ inst.opcode, getPc().* });
        return error.NotImplemented;
    }

    fn instStubSpecial(inst: Instruction) RspError!void {
        const r_type = inst.data.r_type;
        log.err("Unknown SPECIAL function 0b{b} at address 0x{X}!", .{ @enumToInt(r_type.function), getPc().* });
        return error.UnknownInstruction;
    }

    fn instAdd(inst: Instruction) RspError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = gpr[r_type.rs] +% gpr[r_type.rt];
        getPc().* +%= 4;
    }

    fn instAddi(inst: Instruction) RspError!void {
        const i_type = inst.data.i_type;
        const immediate = @bitCast(u32, @as(i32, @bitCast(i16, i_type.immediate)));
        gpr[i_type.rt] = gpr[i_type.rs] +% immediate;
        getPc().* +%= 4;
    }

    fn instAnd(inst: Instruction) RspError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = gpr[r_type.rs] & gpr[r_type.rt];
        getPc().* +%= 4;
    }

    fn instAndi(inst: Instruction) RspError!void {
        const i_type = inst.data.i_type;
        gpr[i_type.rt] = gpr[i_type.rs] & i_type.immediate;
        getPc().* +%= 4;
    }

    fn instBeq(inst: Instruction) RspError!void {
        const i_type_branch = inst.data.i_type_branch;
        if (gpr[i_type_branch.rs] == gpr[i_type_branch.rt]) {
            branch(i_type_branch.offset);
        }
        getPc().* +%= 4;
    }

    fn instRegimm(inst: Instruction) RspError!void {
        const i_type_branch = inst.data.i_type_regimm_branch;
        if (i_type_branch.sub_opcode == Instruction.RegimmSubOpcode.bgez) {
            try instBgez(inst);
            return;
        } else if (i_type_branch.sub_opcode == Instruction.RegimmSubOpcode.bgezal) {
            try instBgezal(inst);
            return;
        } else if (i_type_branch.sub_opcode == Instruction.RegimmSubOpcode.bltz) {
            try instBltz(inst);
            return;
        } else if (i_type_branch.sub_opcode == Instruction.RegimmSubOpcode.bltzal) {
            try instBltzal(inst);
            return;
        } else {
            log.err("Unknown REGIMM sub-opcode 0b{b}", .{ @enumToInt(i_type_branch.sub_opcode) });
            return error.UnknownInstruction;
        }
    }

    fn instBgez(inst: Instruction) RspError!void {
        const i_type_branch = inst.data.i_type_branch;
        const rs = @bitCast(i32, gpr[i_type_branch.rs]);
        if (rs >= 0) {
            branch(i_type_branch.offset);
        }
        getPc().* +%= 4;
    }

    fn instBgezal(inst: Instruction) RspError!void {
        const i_type_branch = inst.data.i_type_branch;
        const rs = @bitCast(i32, gpr[i_type_branch.rs]);
        link();
        if (rs >= 0) {
            branch(i_type_branch.offset);
        }
        getPc().* +%= 4;
    }

    fn instBgtz(inst: Instruction) RspError!void {
        const i_type_branch = inst.data.i_type_branch;
        const rs = @bitCast(i32, gpr[i_type_branch.rs]);
        if (rs > 0) {
            branch(i_type_branch.offset);
        }
        getPc().* +%= 4;
    }

    fn instBlez(inst: Instruction) RspError!void {
        const i_type_branch = inst.data.i_type_branch;
        const rs = @bitCast(i32, gpr[i_type_branch.rs]);
        if (rs <= 0) {
            branch(i_type_branch.offset);
        }
        getPc().* +%= 4;
    }

    fn instBltz(inst: Instruction) RspError!void {
        const i_type_branch = inst.data.i_type_branch;
        const rs = @bitCast(i32, gpr[i_type_branch.rs]);
        if (rs < 0) {
            branch(i_type_branch.offset);
        }
        getPc().* +%= 4;
    }

    fn instBltzal(inst: Instruction) RspError!void {
        const i_type_branch = inst.data.i_type_branch;
        const rs = @bitCast(i32, gpr[i_type_branch.rs]);
        link();
        if (rs < 0) {
            branch(i_type_branch.offset);
        }
        getPc().* +%= 4;
    }

    fn instBne(inst: Instruction) RspError!void {
        const i_type_branch = inst.data.i_type_branch;
        if (gpr[i_type_branch.rs] != gpr[i_type_branch.rt]) {
            branch(i_type_branch.offset);
        }
        getPc().* +%= 4;
    }

    fn instBreak(_: Instruction) RspError!void {
        memory.rcp.rsp.getSpStatusFlags().halt = true;
        memory.rcp.rsp.getSpStatusFlags().broke = true;
        if (memory.rcp.rsp.getSpStatusFlags().interrupt_on_break) {
            memory.rcp.mi.getMiInterruptFlags().sp = true;
        }
    }

    // TODO: Implement CFC2 and CTC2

    fn instJ(inst: Instruction) RspError!void {
        const j_type = inst.data.j_type;
        jump(j_type.target);
        getPc().* +%= 4;
    }

    fn instJal(inst: Instruction) RspError!void {
        const j_type = inst.data.j_type;
        link();
        jump(j_type.target);
        getPc().* +%= 4;
    }

    fn instJalr(inst: Instruction) RspError!void {
        const r_type = inst.data.r_type;
        linkReg(r_type.rd);
        jumpReg(r_type.rs);
        getPc().* +%= 4;
    }

    fn instJr(inst: Instruction) RspError!void {
        const r_type = inst.data.r_type;
        jumpReg(r_type.rs);
        getPc().* +%= 4;
    }

    fn instLb(inst: Instruction) RspError!void {
        const i_type = inst.data.i_type_mem;
        const address = calculateAddress(i_type.base, i_type.offset);
        const result = memory.rcp.rsp.dmem.readAligned(u8, address);
        gpr[i_type.rt] = @bitCast(u32, @as(i32, @bitCast(i8, result)));
        getPc().* +%= 4;
    }

    fn instLbu(inst: Instruction) RspError!void {
        const i_type = inst.data.i_type_mem;
        const address = calculateAddress(i_type.base, i_type.offset);
        const result = memory.rcp.rsp.dmem.readAligned(u8, address);
        gpr[i_type.rt] = result;
        getPc().* +%= 4;
    }

    // TODO: Implement LBV, LDV, and LFV.

    fn instLh(inst: Instruction) RspError!void {
        const i_type = inst.data.i_type_mem;
        const address = calculateAddress(i_type.base, i_type.offset);
        // Seems to allow unaligned addresses.
        const result = 
            (@as(u16, memory.rcp.rsp.dmem.readAligned(u8, address)) << 8) | 
             @as(u16, memory.rcp.rsp.dmem.readAligned(u8, address + 1));
        gpr[i_type.rt] = result;
        getPc().* +%= 4;
    }

    fn instLhu(inst: Instruction) RspError!void {
        const i_type = inst.data.i_type_mem;
        const address = calculateAddress(i_type.base, i_type.offset);
        // Seems to allow unaligned addresses.
        const result = 
                (@as(u16, memory.rcp.rsp.dmem.readAligned(u8, address)) << 8) | 
                 @as(u16, memory.rcp.rsp.dmem.readAligned(u8, address + 1));
        gpr[i_type.rt] = @bitCast(u32, @as(i32, @bitCast(i16, result)));
        getPc().* +%= 4;
    }

    // TODO: Implement LHV, LLV, LPV, LQV, LRV, LSV, and LTV.

    fn instLui(inst: Instruction) RspError!void {
        const i_type = inst.data.i_type;
        gpr[i_type.rt] = @as(u32, i_type.immediate) << 16;
        getPc().* +%= 4;
    }

    // TODO: Implement LUV

    fn instLw(inst: Instruction) RspError!void {
        const i_type = inst.data.i_type_mem;
        const address = calculateAddress(i_type.base, i_type.offset);
        // Seems to allow unaligned addresses.
        const result = 
            (@as(u32, memory.rcp.rsp.dmem.readAligned(u8, address + 0)) << 24) | 
            (@as(u32, memory.rcp.rsp.dmem.readAligned(u8, address + 1)) << 16) |
            (@as(u32, memory.rcp.rsp.dmem.readAligned(u8, address + 2)) << 8) |
            (@as(u32, memory.rcp.rsp.dmem.readAligned(u8, address + 3)) << 0);
        gpr[i_type.rt] = result;
        getPc().* +%= 4;
    }

    fn instCop0(inst: Instruction) RspError!void {
        const r_type = inst.data.r_type_cop;
        if (r_type.sub_opcode == Instruction.CopSubOpcode.mf) {
            try instMfc0(inst);
            return;
        } else if (r_type.sub_opcode == Instruction.CopSubOpcode.mt) {
            try instMtc0(inst);
            return;
        } else {
            log.err("Unknown COP0 function 0b{b} at address 0x{X}!", .{ @enumToInt(r_type.sub_opcode), getPc().* });
            return error.UnknownInstruction;
        }
    }

    fn getCop0MemRange(rd: u5) RspError!*memory.MemRange {
        return switch(rd) {
            @enumToInt(Cop0Register.sp_mem_addr) => &memory.rcp.rsp.reg_range_0,
            @enumToInt(Cop0Register.sp_dram_addr) => &memory.rcp.rsp.reg_range_0,
            @enumToInt(Cop0Register.sp_rd_len) => &memory.rcp.rsp.reg_range_0,
            @enumToInt(Cop0Register.sp_wr_len) => &memory.rcp.rsp.reg_range_0,
            @enumToInt(Cop0Register.sp_status) => &memory.rcp.rsp.reg_range_0,
            @enumToInt(Cop0Register.sp_dma_full) => &memory.rcp.rsp.reg_range_0,
            @enumToInt(Cop0Register.sp_dma_busy) => &memory.rcp.rsp.reg_range_0,
            @enumToInt(Cop0Register.sp_semaphore) => &memory.rcp.rsp.reg_range_0,
            @enumToInt(Cop0Register.dpc_start) => &memory.rcp.rdp.cmd,
            @enumToInt(Cop0Register.dpc_end) => &memory.rcp.rdp.cmd,
            @enumToInt(Cop0Register.dpc_current) => &memory.rcp.rdp.cmd,
            @enumToInt(Cop0Register.dpc_status) => &memory.rcp.rdp.cmd,
            @enumToInt(Cop0Register.dpc_clock) => &memory.rcp.rdp.cmd,
            @enumToInt(Cop0Register.dpc_busy) => &memory.rcp.rdp.cmd,
            @enumToInt(Cop0Register.dpc_pipe_busy) => &memory.rcp.rdp.cmd,
            @enumToInt(Cop0Register.dpc_tmem_busy) => &memory.rcp.rdp.cmd,
            else => return error.InvalidInstruction
        };
    }

    fn getCop0MemOffset(rd: u5) RspError!u32 {
        return switch(rd) {
            @enumToInt(Cop0Register.sp_mem_addr) => @enumToInt(memory.rcp.rsp.RegRange0Offset.sp_mem_addr_reg),
            @enumToInt(Cop0Register.sp_dram_addr) => @enumToInt(memory.rcp.rsp.RegRange0Offset.sp_dram_addr_reg),
            @enumToInt(Cop0Register.sp_rd_len) => @enumToInt(memory.rcp.rsp.RegRange0Offset.sp_rd_len_reg),
            @enumToInt(Cop0Register.sp_wr_len) => @enumToInt(memory.rcp.rsp.RegRange0Offset.sp_wr_len_reg),
            @enumToInt(Cop0Register.sp_status) => @enumToInt(memory.rcp.rsp.RegRange0Offset.sp_status_reg),
            @enumToInt(Cop0Register.sp_dma_full) => @enumToInt(memory.rcp.rsp.RegRange0Offset.sp_dma_full_reg),
            @enumToInt(Cop0Register.sp_dma_busy) => @enumToInt(memory.rcp.rsp.RegRange0Offset.sp_dma_busy_reg),
            @enumToInt(Cop0Register.sp_semaphore) => @enumToInt(memory.rcp.rsp.RegRange0Offset.sp_semaphore_reg),
            @enumToInt(Cop0Register.dpc_start) => @enumToInt(memory.rcp.rdp.CmdRangeOffset.dpc_start_reg),
            @enumToInt(Cop0Register.dpc_end) => @enumToInt(memory.rcp.rdp.CmdRangeOffset.dpc_end_reg),
            @enumToInt(Cop0Register.dpc_current) => @enumToInt(memory.rcp.rdp.CmdRangeOffset.dpc_current_reg),
            @enumToInt(Cop0Register.dpc_status) => @enumToInt(memory.rcp.rdp.CmdRangeOffset.dpc_status_reg),
            @enumToInt(Cop0Register.dpc_clock) => @enumToInt(memory.rcp.rdp.CmdRangeOffset.dpc_clock_reg),
            @enumToInt(Cop0Register.dpc_busy) => @enumToInt(memory.rcp.rdp.CmdRangeOffset.dpc_bufbusy_reg),
            @enumToInt(Cop0Register.dpc_pipe_busy) => @enumToInt(memory.rcp.rdp.CmdRangeOffset.dpc_pipebusy_reg),
            @enumToInt(Cop0Register.dpc_tmem_busy) => @enumToInt(memory.rcp.rdp.CmdRangeOffset.dpc_tmem_reg),
            else => return error.InvalidInstruction
        };
    }

    fn instMfc0(inst: Instruction) RspError!void {
        const r_type = inst.data.r_type_cop;
        const mem_range: *memory.MemRange = try getCop0MemRange(r_type.rd);
        const mem_offset: u32 = try getCop0MemOffset(r_type.rd);
        gpr[r_type.rt] = mem_range.readAligned(u32, mem_offset);
        getPc().* +%= 4;
    }

    fn instMtc0(inst: Instruction) RspError!void {
        const r_type = inst.data.r_type_cop;
        const mem_range: *memory.MemRange = try getCop0MemRange(r_type.rd);
        const mem_offset: u32 = try getCop0MemOffset(r_type.rd);
        mem_range.writeAligned(mem_offset, gpr[r_type.rt]);
        getPc().* +%= 4;
    }

    // TODO: Implement MTC2

    fn instNor(inst: Instruction) RspError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = ~(gpr[r_type.rs] | gpr[r_type.rt]);
        getPc().* +%= 4;
    }

    fn instOr(inst: Instruction) RspError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = gpr[r_type.rs] | gpr[r_type.rt];
        getPc().* +%= 4;
    }

    fn instOri(inst: Instruction) RspError!void {
        const i_type = inst.data.i_type;
        gpr[i_type.rt] = gpr[i_type.rs] | @as(u32, i_type.immediate);
        getPc().* +%= 4;
    }

    fn instSb(inst: Instruction) RspError!void {
        const i_type = inst.data.i_type_mem;
        const address = calculateAddress(i_type.base, i_type.offset);
        memory.rcp.rsp.dmem.writeAligned(address, @truncate(u8, gpr[i_type.rt]));
        getPc().* +%= 4;
    }

    // TODO: Implement SBV, SDV, and SFV.

    fn instSh(inst: Instruction) RspError!void {
        const i_type = inst.data.i_type_mem;
        const address = calculateAddress(i_type.base, i_type.offset);
        // Seems to allow unaligned addresses.
        memory.rcp.rsp.dmem.writeAligned(address + 0, @truncate(u8, gpr[i_type.rt] >> 8));
        memory.rcp.rsp.dmem.writeAligned(address + 1, @truncate(u8, gpr[i_type.rt] >> 0));
        getPc().* +%= 4;
    }

    // TODO: Implement SHV

    fn instSll(inst: Instruction) RspError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = gpr[r_type.rt] << r_type.sa;
        getPc().* +%= 4;
    }

    fn instSllv(inst: Instruction) RspError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = gpr[r_type.rt] << @truncate(u5, gpr[r_type.rs]);
        getPc().* +%= 4;
    }

    fn instSlt(inst: Instruction) RspError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = @boolToInt(@bitCast(i32, gpr[r_type.rs]) < @bitCast(i32, gpr[r_type.rt]));
        getPc().* +%= 4;
    }

    fn instSlti(inst: Instruction) RspError!void {
        const i_type = inst.data.i_type;
        gpr[i_type.rt] = @boolToInt(@bitCast(i32, gpr[i_type.rs]) < @bitCast(i16, i_type.immediate));
        getPc().* +%= 4;
    }

    fn instSltiu(inst: Instruction) RspError!void {
        const i_type = inst.data.i_type;
        gpr[i_type.rt] = @boolToInt(gpr[i_type.rs] < @bitCast(i16, i_type.immediate));
        getPc().* +%= 4;
    }

    fn instSltu(inst: Instruction) RspError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = @boolToInt(gpr[r_type.rs] < gpr[r_type.rt]);
        getPc().* +%= 4;
    }

    // TODO: Implement SLV, SPV, and SQV.

    fn instSra(inst: Instruction) RspError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = @bitCast(u32, @bitCast(i32, gpr[r_type.rt]) >> r_type.sa);
        getPc().* +%= 4;
    }

    fn instSrav(inst: Instruction) RspError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = @bitCast(u32, @bitCast(i32, gpr[r_type.rt]) >> @truncate(u5, gpr[r_type.rs]));
        getPc().* +%= 4;
    }

    fn instSrl(inst: Instruction) RspError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = gpr[r_type.rt] >> r_type.sa;
        getPc().* +%= 4;
    }

    fn instSrlv(inst: Instruction) RspError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = gpr[r_type.rt] >> @truncate(u5, gpr[r_type.rs]);
        getPc().* +%= 4;
    }

    // TODO: Implement SRV, SSV, and STV.

    fn instSub(inst: Instruction) RspError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = gpr[r_type.rs] -% gpr[r_type.rt];
    }

    // TODO: Implement SUV.

    fn instSw(inst: Instruction) RspError!void {
        const i_type = inst.data.i_type_mem;
        const address = calculateAddress(i_type.base, i_type.offset);
        // Seems to allow unaligned addresses.
        memory.rcp.rsp.dmem.writeAligned(address + 0, @truncate(u8, gpr[i_type.rt] >> 24));
        memory.rcp.rsp.dmem.writeAligned(address + 1, @truncate(u8, gpr[i_type.rt] >> 16));
        memory.rcp.rsp.dmem.writeAligned(address + 2, @truncate(u8, gpr[i_type.rt] >> 8));
        memory.rcp.rsp.dmem.writeAligned(address + 3, @truncate(u8, gpr[i_type.rt] >> 0));
        getPc().* +%= 4;
    }

    // TODO: Implement SWV.
    // TODO: Implement all VU Instructions.

    fn instXor(inst: Instruction) RspError!void {
        const r_type = inst.data.r_type;
        gpr[r_type.rd] = gpr[r_type.rs] ^ gpr[r_type.rt];
        getPc().* +%= 4;
    }

    fn instXori(inst: Instruction) RspError!void {
        const i_type = inst.data.i_type;
        gpr[i_type.rt] = gpr[i_type.rs] ^ @as(u32, i_type.immediate);
        getPc().* +%= 4;
    }
};
