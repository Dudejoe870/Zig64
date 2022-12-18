const std = @import("std");
const builtin = @import("builtin");

const system = @import("system.zig");
const ui = @import("ui.zig");
const vi_renderer = @import("vi_renderer.zig");

const c = @import("c.zig");

const glfw = @import("glfw");
const gl = @import("gl");

const dummy_stack_protector = @import("dummy_stack_protector.zig");

fn glGetProcAddress(p: glfw.GLProc, proc: [:0]const u8) ?gl.FunctionPointer {
    _ = p;
    return glfw.getProcAddress(proc);
}

pub fn main() !u8 {
    // These symbols are required to be defined by GLFW 
    // in release-fast mode for some reason.
    if (builtin.mode == std.builtin.Mode.ReleaseFast) {
        _ = dummy_stack_protector.__stack_chk_fail;
        _ = dummy_stack_protector.__stack_chk_guard;
    }

    var arena = std.heap.ArenaAllocator.init(std.heap.raw_c_allocator);
    defer arena.deinit();

    var arg_it = try std.process.argsWithAllocator(arena.allocator());
    _ = arg_it.skip();

    const rom = arg_it.next() orelse {
        std.log.err("Expected first argument to be rom to run", .{ });
        return error.InvalidArgs;
    };
    const pif = arg_it.next();

    try runEmulator(pif, rom);
    return 0;
}

var running: bool = true;

fn runSystem() !void {
    while (running) {
        try system.step();
    }
}

fn runEmulator(bootrom_path: ?[]const u8, rom_path: []const u8) !void {
    try system.init(bootrom_path, rom_path);

    var system_thread = try std.Thread.spawn(.{ }, runSystem, .{ });
    try system_thread.setName("Emulator Thread");

    try glfw.init(.{});
    defer glfw.terminate();

    const window = try glfw.Window.create(640, 480, "Zig64", null, null, .{
        .opengl_profile = .opengl_core_profile,
        .context_version_major = 3,
        .context_version_minor = 3
    });
    defer window.destroy();

    try glfw.makeContextCurrent(window);

    const proc: glfw.GLProc = undefined;
    try gl.load(proc, glGetProcAddress);

    try vi_renderer.init();
    defer vi_renderer.deinit();

    _ = c.igCreateContext(null);

    var io = c.igGetIO();
    io.*.ConfigFlags |= c.ImGuiConfigFlags_NavEnableGamepad;

    _ = c.ImGui_ImplGlfw_InitForOpenGL(@ptrCast(*c.GLFWwindow, window.handle), true);
    _ = c.ImGui_ImplOpenGL3_Init("#version 150");

    c.igStyleColorsDark(null);

    while (!window.shouldClose()) {
        try glfw.pollEvents();

        c.ImGui_ImplOpenGL3_NewFrame();
        c.ImGui_ImplGlfw_NewFrame();
        c.igNewFrame();

        try ui.renderUI();

        c.igRender();
        
        gl.viewport(0, 0, @floatToInt(gl.GLint, io.*.DisplaySize.x), @floatToInt(gl.GLint, io.*.DisplaySize.y));
        gl.clearColor(0, 0, 0, 1);
        gl.clear(gl.COLOR_BUFFER_BIT);

        try vi_renderer.render();

        c.ImGui_ImplOpenGL3_RenderDrawData(c.igGetDrawData());

        try window.swapBuffers();
    }
    running = false;
    system_thread.join();

    c.ImGui_ImplOpenGL3_Shutdown();
    c.ImGui_ImplGlfw_Shutdown();
    c.igDestroyContext(null);
}
