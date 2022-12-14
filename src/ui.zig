const std = @import("std");
const c = @import("c.zig");

const system = @import("system.zig");
const r4300 = @import("r4300.zig");

var text_buffer: [1024]u8 = undefined;

pub fn renderUI() !void {
    try renderCpuInspector();
    try renderCpuDebugger();
    try renderSystemControl();
}

fn renderCpuDebugger() !void {
    if (c.igBegin("CPU Debugger", null, 0)) {
        
    }
    c.igEnd();
}

fn renderSystemControl() !void {
    if (c.igBegin("System Control", null, 0)) {
        _ = c.igCheckbox("Paused", &system.paused);
        c.igSameLine(0.0, -1.0);
        if (c.igButton("Step", c.ImVec2 { .x = 0.0, .y = 0.0 })) {
            system.singleStep();
        }

        if (c.igBeginCombo("Single-step Setting", @tagName(system.getSingleStepSetting()).ptr, 0)) {
            comptime var setting_enum = @typeInfo(system.SingleStepSetting);
            inline for (setting_enum.Enum.fields) |f| {
                var is_selected = @enumToInt(system.getSingleStepSetting()) == f.value;
                if (c.igSelectable_Bool(f.name.ptr, is_selected, 0, .{ .x = 0.0, .y = 0.0 })) {
                    system.setSingleStepSetting(@intToEnum(system.SingleStepSetting, f.value));
                }
                if (is_selected) {
                    c.igSetItemDefaultFocus();
                }
            }
            c.igEndCombo();
        }
    }
    c.igEnd();
}

fn renderCpuInspector() !void {
    if (c.igBegin("CPU Inspector", null, 0)) {
        c.igTextUnformatted((try std.fmt.bufPrint(text_buffer[0..], 
            "pc = 0x{X}\x00", .{ r4300.pc })).ptr, null);

        if (c.igCollapsingHeader_TreeNodeFlags("General Purpose Registers", 0)) {
            for (r4300.gpr) |reg, i| {
                c.igTextUnformatted((try std.fmt.bufPrint(text_buffer[0..], 
                    "gpr[{d}] = 0x{X} ({d}u {d})\x00", 
                    .{ i, reg, reg, @bitCast(i64, reg) })).ptr, null);
            }
        }

        if (c.igCollapsingHeader_TreeNodeFlags("Coprocessor 0 Registers", 0)) {
            for (r4300.cp0) |reg, i| {
                c.igTextUnformatted((try std.fmt.bufPrint(text_buffer[0..], 
                    "cp0[{s}] = 0x{X} ({d}u {d})\x00", 
                    .{ @tagName(@intToEnum(r4300.Cop0Register, i)), reg, reg, @bitCast(i32, reg) })).ptr, null);
            }
        }

        if (c.igCollapsingHeader_TreeNodeFlags("Coprocessor 1 (FPU) Registers", 0)) {
            c.igTreePush_Ptr(null);
            if (c.igCollapsingHeader_TreeNodeFlags("Control Registers", 0)) {
                c.igTextUnformatted((try std.fmt.bufPrint(text_buffer[0..], 
                    "fcr[0] = 0x{X} ({d}u {d})\x00", 
                    .{ r4300.fcr0, r4300.fcr0, r4300.fcr0 })).ptr, null);
                c.igTextUnformatted((try std.fmt.bufPrint(text_buffer[0..], 
                    "fcr[31] = 0x{X} ({d}u {d})\x00", 
                    .{ r4300.fcr31, r4300.fcr31, r4300.fcr31 })).ptr, null);
            }

            if (c.igCollapsingHeader_TreeNodeFlags("Integer", 0)) {
                for (r4300.fpr) |reg, i| {
                    c.igTextUnformatted((try std.fmt.bufPrint(text_buffer[0..], 
                        "fpr[{d}] = 0x{X} ({d}u {d})\x00", 
                        .{ i, reg, reg, @bitCast(i64, reg) })).ptr, null);
                }
            }

            if (c.igCollapsingHeader_TreeNodeFlags("Float", c.ImGuiTreeNodeFlags_DefaultOpen)) {
                for (r4300.fpr) |reg, i| {
                    c.igTextUnformatted((try std.fmt.bufPrint(text_buffer[0..], 
                        "fpr[{d}] = {d}\x00", 
                        .{ i, @bitCast(f32, @truncate(u32, reg)) })).ptr, null);
                }
            }

            if (c.igCollapsingHeader_TreeNodeFlags("Double", 0)) {
                for (r4300.fpr) |reg, i| {
                    c.igTextUnformatted((try std.fmt.bufPrint(text_buffer[0..], 
                        "fpr[{d}] = {d}\x00", 
                        .{ i, @bitCast(f64, reg) })).ptr, null);
                }
            }
            c.igTreePop();
        }
    }
    c.igEnd();
}
