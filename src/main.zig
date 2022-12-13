const std = @import("std");

const System = @import("System.zig");

const glfw = @import("glfw");
const gl = @import("gl");

fn glGetProcAddress(p: glfw.GLProc, proc: [:0]const u8) ?gl.FunctionPointer {
    _ = p;
    return glfw.getProcAddress(proc);
}

pub fn main() !u8 {
    var arena = std.heap.ArenaAllocator.init(std.heap.raw_c_allocator);
    defer arena.deinit();

    var arg_it = try std.process.argsWithAllocator(arena.allocator());
    _ = arg_it.skip();

    const rom = arg_it.next() orelse {
        std.log.err("Expected first argument to be rom to run", .{ });
        return error.InvalidArgs;
    };
    const pif = arg_it.next() orelse {
        std.log.err("Expected first argument to be PIF rom.", .{ });
        return error.InvalidArgs;
    };

    try runEmulator(pif, rom);
    return 0;
}

fn runEmulator(bootrom_path: ?[]const u8, rom_path: []const u8) !void {
    try glfw.init(.{});
    defer glfw.terminate();

    const window = try glfw.Window.create(640, 480, "Zig64", null, null, .{
        .opengl_profile = .opengl_core_profile,
        .context_version_major = 4,
        .context_version_minor = 6,
    });
    defer window.destroy();

    try glfw.makeContextCurrent(window);

    const proc: glfw.GLProc = undefined;
    try gl.load(proc, glGetProcAddress);

    try System.init(bootrom_path, rom_path);

    while (!window.shouldClose()) {
        try glfw.pollEvents();

        gl.clearColor(0, 0, 0, 1);
        gl.clear(gl.COLOR_BUFFER_BIT);

        try System.step();

        try window.swapBuffers();
    }
}
