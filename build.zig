const Builder = @import("std").build.Builder;

const glfw = @import("external/mach-glfw/build.zig");

pub fn build(b: *Builder) !void {
    const target = b.standardTargetOptions(.{});
    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable("zig64", "src/main.zig");

    exe.addPackagePath("gl", "opengl/gl_4v6.zig");

    exe.addPackage(glfw.pkg);
    try glfw.link(b, exe, .{ });

    exe.setTarget(target);
    exe.setBuildMode(mode);

    exe.linkLibC();
    
    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
