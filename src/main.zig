const std = @import("std");

const System = @import("System.zig");

const glfw = @import("glfw");
const gl = @import("gl");

pub const imgui = @cImport({
    @cDefine("CIMGUI_USE_GLFW", {});
    @cDefine("CIMGUI_USE_OPENGL3", {});
    @cDefine("CIMGUI_NO_EXPORT", {});
    @cDefine("CIMGUI_DEFINE_ENUMS_AND_STRUCTS", {});

    @cInclude("cimgui.h");
    @cInclude("generator/output/cimgui_impl.h");
});

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

var running: bool = true;

fn runSystem() !void {
    while (running) {
        try System.step();
    }
}

fn runEmulator(bootrom_path: ?[]const u8, rom_path: []const u8) !void {
    try System.init(bootrom_path, rom_path);

    var system_thread = try std.Thread.spawn(.{ }, runSystem, .{ });
    try system_thread.setName("Emulator Thread");

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

    _ = imgui.igCreateContext(null);

    var io = imgui.igGetIO();
    io.*.ConfigFlags |= imgui.ImGuiConfigFlags_NavEnableGamepad;

    _ = imgui.ImGui_ImplGlfw_InitForOpenGL(@ptrCast(*imgui.GLFWwindow, window.handle), true);
    _ = imgui.ImGui_ImplOpenGL3_Init("#version 150");

    imgui.igStyleColorsDark(null);

    while (!window.shouldClose()) {
        try glfw.pollEvents();

        imgui.ImGui_ImplOpenGL3_NewFrame();
        imgui.ImGui_ImplGlfw_NewFrame();
        imgui.igNewFrame();

        imgui.igShowDemoWindow(null);

        imgui.igRender();
        try glfw.makeContextCurrent(window);
        gl.viewport(0, 0, @floatToInt(gl.GLint, io.*.DisplaySize.x), @floatToInt(gl.GLint, io.*.DisplaySize.y));
        gl.clearColor(0, 0, 0, 1);
        gl.clear(gl.COLOR_BUFFER_BIT);
        imgui.ImGui_ImplOpenGL3_RenderDrawData(imgui.igGetDrawData());

        try window.swapBuffers();
    }
    running = false;
    system_thread.join();

    imgui.ImGui_ImplOpenGL3_Shutdown();
    imgui.ImGui_ImplGlfw_Shutdown();
    imgui.igDestroyContext(null);
}
