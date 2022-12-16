const Builder = @import("std").build.Builder;

const glfw = @import("external/mach-glfw/build.zig");

pub fn build(b: *Builder) !void {
    const target = b.standardTargetOptions(.{ });
    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable("zig64", "src/main.zig");

    exe.linkLibC();
    exe.linkLibCpp();

    exe.addIncludePath("external/cimgui");
    exe.addIncludePath("external/cimgui/imgui");

    exe.addCSourceFiles(&[_][]const u8 {
        "external/cimgui/imgui/imgui.cpp",
        "external/cimgui/imgui/imgui_draw.cpp",
        "external/cimgui/imgui/imgui_demo.cpp",
        "external/cimgui/imgui/imgui_widgets.cpp",
        "external/cimgui/imgui/imgui_tables.cpp",
        "external/cimgui/imgui/backends/imgui_impl_glfw.cpp",
        "external/cimgui/imgui/backends/imgui_impl_opengl3.cpp",
        "external/cimgui/cimgui.cpp"
    }, &[_][]const u8 { 
        "-DIMGUI_DISABLE_OBSOLETE_FUNCTIONS=1",
        "-DIMGUI_IMPL_API=extern \"C\"",
        "-DIMGUI_IMPL_OPENGL_LOADER_GL3W"
    });
    exe.addPackagePath("gl", "opengl/gl_4v6.zig");

    exe.addPackage(glfw.pkg);
    try glfw.link(b, exe, .{ .opengl = true });

    exe.setTarget(target);
    exe.setBuildMode(mode);
    
    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
