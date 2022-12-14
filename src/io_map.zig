const std = @import("std");
const memory = @import("memory.zig");

const log = std.log.scoped(.IoMap);

pub const rdram_base_addr: u32 = 0x00000000;
pub const rdram_reg_base_addr: u32 = 0x03f00000;

pub const sp_dmem_base_addr: u32 = 0x04000000;
pub const sp_imem_base_addr: u32 = 0x04001000;
pub const sp_reg_range_0_base_addr: u32 = 0x04040000;
pub const sp_reg_range_1_base_addr: u32 = 0x04080000;

pub const dpc_base_addr: u32 = 0x04100000;
pub const dps_base_addr: u32 = 0x04200000;
pub const mi_base_addr: u32 = 0x04300000;
pub const vi_base_addr: u32 = 0x04400000;
pub const ai_base_addr: u32 = 0x04500000;
pub const pi_base_addr: u32 = 0x04600000;
pub const ri_base_addr: u32 = 0x04700000;
pub const si_base_addr: u32 = 0x04800000;

pub const cart_dom1_addr2_base_addr: u32 = 0x10000000;
pub const pif_boot_rom_base_addr: u32 = 0x1fc00000;
pub const pif_ram_base_addr: u32 = 0x1fc007c0;

// Everything is compile-time evaluated into individual checks at run-time.
// TODO: Replace this with binary search.
fn MemoryEntry(comptime T: type) type {
    return struct {
        memory: *T,
        address: u32
    };
}

const memory_entries = .{ 
    MemoryEntry(@TypeOf(memory.rdram.dram)) { .address = rdram_base_addr, .memory = &memory.rdram.dram },
    MemoryEntry(@TypeOf(memory.rdram.reg_range)) { .address = rdram_reg_base_addr, .memory = &memory.rdram.reg_range },
    MemoryEntry(@TypeOf(memory.rcp.rsp.dmem)) { .address = sp_dmem_base_addr, .memory = &memory.rcp.rsp.dmem },
    MemoryEntry(@TypeOf(memory.rcp.rsp.imem)) { .address = sp_imem_base_addr, .memory = &memory.rcp.rsp.imem },
    MemoryEntry(@TypeOf(memory.rcp.rsp.reg_range_0)) { .address = sp_reg_range_0_base_addr, .memory = &memory.rcp.rsp.reg_range_0 },
    MemoryEntry(@TypeOf(memory.rcp.rsp.reg_range_1)) { .address = sp_reg_range_1_base_addr, .memory = &memory.rcp.rsp.reg_range_1 },
    MemoryEntry(@TypeOf(memory.rcp.rdp.cmd)) { .address = dpc_base_addr, .memory = &memory.rcp.rdp.cmd },
    MemoryEntry(@TypeOf(memory.rcp.rdp.span)) { .address = dps_base_addr, .memory = &memory.rcp.rdp.span },
    MemoryEntry(@TypeOf(memory.rcp.mi.reg_range)) { .address = mi_base_addr, .memory = &memory.rcp.mi.reg_range },
    MemoryEntry(@TypeOf(memory.rcp.vi.reg_range)) { .address = vi_base_addr, .memory = &memory.rcp.vi.reg_range },
    MemoryEntry(@TypeOf(memory.rcp.ai.reg_range)) { .address = ai_base_addr, .memory = &memory.rcp.ai.reg_range },
    MemoryEntry(@TypeOf(memory.rcp.pi.reg_range)) { .address = pi_base_addr, .memory = &memory.rcp.pi.reg_range },
    MemoryEntry(@TypeOf(memory.rcp.ri.reg_range)) { .address = ri_base_addr, .memory = &memory.rcp.ri.reg_range },
    MemoryEntry(@TypeOf(memory.rcp.si.reg_range)) { .address = si_base_addr, .memory = &memory.rcp.si.reg_range },
    MemoryEntry(@TypeOf(memory.cart.rom)) { .address = cart_dom1_addr2_base_addr, .memory = &memory.cart.rom },
    MemoryEntry(@TypeOf(memory.pif.boot_rom)) { .address = pif_boot_rom_base_addr, .memory = &memory.pif.boot_rom },
    MemoryEntry(@TypeOf(memory.pif.ram)) { .address = pif_ram_base_addr, .memory = &memory.pif.ram }
};

pub inline fn writeAligned(physical_address: u32, value: anytype) void {
    const T = @TypeOf(value);

    comptime {
        std.debug.assert(T == u8 or T == u16 or T == u32 or T == u64);
    }

    inline for (memory_entries) |e| {
        if (physical_address >= e.address and physical_address < e.address + e.memory.buf.len) {
            e.memory.writeAligned(physical_address - e.address, value);
            return;
        }
    }

    log.warn("Trying to write to unmapped memory at address 0x{X}!", .{ physical_address });
    return;
}

pub inline fn readAligned(comptime T: type, physical_address: u32) T {
    comptime {
        std.debug.assert(T == u8 or T == u16 or T == u32 or T == u64);
    }

    inline for (memory_entries) |e| {
        if (physical_address >= e.address and physical_address < e.address + e.memory.buf.len) {
            return e.memory.readAligned(T, physical_address - e.address);
        }
    }

    log.warn("Trying to read from unmapped memory at address 0x{X}!", .{ physical_address });
    return @as(T, 0);
}
