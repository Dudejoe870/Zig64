const std = @import("std");

const cpu_bus = @import("cpu_bus.zig");
const memory = @import("memory.zig");

const gl = @import("gl");

const log = std.log.scoped(.vi_renderer);

const vertex_shader_source = @embedFile("shaders/vertex.vert");
const fragment_shader_source = @embedFile("shaders/fragment.frag");

var framebuffer_tex: gl.GLuint = 0;

var quad_vao: gl.GLuint = 0;
var quad_vbo: gl.GLuint = 0;
var quad_ebo: gl.GLuint = 0;

var quad_program: gl.GLuint = 0;

var framebuffer_swap_buffer: ?[]u8 = null;

const quad_vertices = [_]f32 {
     // Positions     // Texture Coords
     1.0,  1.0, 0.0,  1.0, 0.0, // Top right
     1.0, -1.0, 0.0,  1.0, 1.0, // Bottom right
    -1.0, -1.0, 0.0,  0.0, 1.0, // Bottom left
    -1.0,  1.0, 0.0,  0.0, 0.0  // Top left 
};

const quad_indices = [_]u16 {
    0, 1, 3, // First Triangle
    1, 2, 3 // Second Triangle
};

const ShaderError = error {
    FailedCompileShader,
    FailedLinkProgram
};

const ShaderType = enum {
    Shader,
    Program
};

fn checkCompileErrors(shader: gl.GLuint, comptime shader_type: ShaderType, comptime error_prefix: []const u8) ShaderError!void {
    var success: gl.GLint = 0;
    var info_log: [1024]gl.GLchar = undefined;
    var length: gl.GLsizei = 0;
    if (shader_type == .Shader) {
        gl.getShaderiv(shader, gl.COMPILE_STATUS, &success);
        if (success != 1) {
            gl.getShaderInfoLog(shader, info_log.len, &length, &info_log);
            log.err(error_prefix ++ "{s}", .{ info_log[0..@bitCast(usize, @as(isize, length))] });
            return error.FailedCompileShader;
        }
    } else if (shader_type == .Program) {
        gl.getProgramiv(shader, gl.LINK_STATUS, &success);
        if (success != 1) {
            gl.getProgramInfoLog(shader, info_log.len, &length, &info_log);
            log.err(error_prefix ++ "{s}", .{ info_log[0..@bitCast(usize, @as(isize, length))] });
            return error.FailedLinkProgram;
        }
    }
}

pub fn init() ShaderError!void {
    gl.genTextures(1, &framebuffer_tex);
    gl.bindTexture(gl.TEXTURE_2D, framebuffer_tex);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);

    gl.genVertexArrays(1, &quad_vao);
    gl.genBuffers(1, &quad_vbo);
    gl.genBuffers(1, &quad_ebo);

    gl.bindVertexArray(quad_vao);

    gl.bindBuffer(gl.ARRAY_BUFFER, quad_vbo);
    gl.bufferData(gl.ARRAY_BUFFER, @sizeOf(f32) * quad_vertices.len, &quad_vertices, gl.STATIC_DRAW);

    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, quad_ebo);
    gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, @sizeOf(u16) * quad_indices.len, &quad_indices, gl.STATIC_DRAW);

    gl.vertexAttribPointer(0, 3, gl.FLOAT, gl.FALSE, 5 * @sizeOf(f32), null);
    gl.enableVertexAttribArray(0);
    gl.vertexAttribPointer(1, 2, gl.FLOAT, gl.FALSE, 5 * @sizeOf(f32), @intToPtr(*anyopaque, 3 * @sizeOf(f32)));
    gl.enableVertexAttribArray(1);

    var vertex_shader: gl.GLuint = gl.createShader(gl.VERTEX_SHADER);
    gl.shaderSource(vertex_shader, 1, @ptrCast([*c]const [*c]const u8, &vertex_shader_source), null);
    gl.compileShader(vertex_shader);
    try checkCompileErrors(vertex_shader, .Shader, "Vertex Shader Error: ");

    var fragment_shader: gl.GLuint = gl.createShader(gl.FRAGMENT_SHADER);
    gl.shaderSource(fragment_shader, 1, @ptrCast([*c]const [*c]const u8, &fragment_shader_source), null);
    gl.compileShader(fragment_shader);
    try checkCompileErrors(fragment_shader, .Shader, "Fragment Shader Error: ");

    quad_program = gl.createProgram();
    gl.attachShader(quad_program, vertex_shader);
    gl.attachShader(quad_program, fragment_shader);
    gl.linkProgram(quad_program);
    try checkCompileErrors(quad_program, .Program, "Program Link Error: ");

    gl.deleteShader(fragment_shader);
    gl.deleteShader(vertex_shader);

    gl.uniform1i(gl.getUniformLocation(quad_program, "tex"), 0);
}

pub fn deinit() void {
    gl.deleteProgram(quad_program);

    gl.deleteBuffers(1, &quad_ebo);
    gl.deleteBuffers(1, &quad_vbo);
    gl.deleteVertexArrays(1, &quad_vao);

    gl.deleteTextures(1, &framebuffer_tex);
    if (framebuffer_swap_buffer) |swap_buffer| {
        std.heap.c_allocator.free(swap_buffer);
    }
}

pub fn render() !void {
    var framebuffer_width = memory.rcp.vi.reg_range.getWordPtr(
        @enumToInt(memory.rcp.vi.RegRangeOffset.vi_width_reg)).*;
    // Based off of PeterLemons N64 test suite library.
    var framebuffer_height = ((memory.rcp.vi.reg_range.getWordPtr(
        @enumToInt(memory.rcp.vi.RegRangeOffset.vi_y_scale_reg)).* & 0xFFF) * 60) / 0x100;
    
    var framebuffer_pixel_size = memory.rcp.vi.getViStatusFlags().getPixelSize();

    var framebuffer_length = (framebuffer_width * framebuffer_height) * framebuffer_pixel_size;
    if (framebuffer_length == 0) return;

    var framebuffer_offset = @truncate(u29, memory.rcp.vi.reg_range.getWordPtr(
        @enumToInt(memory.rcp.vi.RegRangeOffset.vi_origin_reg)).*) - cpu_bus.rdram_base_addr;
    if (framebuffer_offset >= memory.rdram.dram_size or 
        framebuffer_offset + framebuffer_length >= memory.rdram.dram_size) return;
    
    const framebuffer_pixel_type: gl.GLenum = switch (framebuffer_pixel_size) {
        2 => gl.UNSIGNED_SHORT_5_5_5_1,
        4 => gl.UNSIGNED_INT_8_8_8_8,
        else => unreachable
    };
    const framebuffer_internal_format: gl.GLint = switch (framebuffer_pixel_size) {
        2 => gl.RGB5_A1,
        4 => gl.RGBA8,
        else => unreachable
    };
    var framebuffer_data = memory.rdram.dram.buf[framebuffer_offset..framebuffer_offset+framebuffer_length];
    gl.bindTexture(gl.TEXTURE_2D, framebuffer_tex);
    if (framebuffer_pixel_size == 2) {
        if (framebuffer_swap_buffer == null or framebuffer_swap_buffer.?.len != framebuffer_length) {
            if (framebuffer_swap_buffer) |swap_buffer| {
                std.heap.c_allocator.free(swap_buffer);
            }
            framebuffer_swap_buffer = try std.heap.c_allocator.alloc(u8, framebuffer_length);
        }

        // We swap the pixels around in 16BPP mode, as the way the are stored in memory 
        // means normally every 2 pixels would be swapped around the way OpenGL reads them.
        const output = std.mem.bytesAsSlice(u32, framebuffer_swap_buffer.?);
        const input = std.mem.bytesAsSlice(u16, framebuffer_data);
        for (output) |*combined_pixels, i| {
            combined_pixels.* = @as(u32, input[(i * 2) + 1]) | (@as(u32, input[i * 2]) << 16);
        }

        gl.texImage2D(gl.TEXTURE_2D, 0, 
            framebuffer_internal_format,
            @bitCast(gl.GLsizei, framebuffer_width), @bitCast(gl.GLsizei, framebuffer_height), 0,
            gl.RGBA, framebuffer_pixel_type, framebuffer_swap_buffer.?.ptr);
    } else {
        if (framebuffer_swap_buffer) |swap_buffer| {
            std.heap.c_allocator.free(swap_buffer);
        }

        gl.texImage2D(gl.TEXTURE_2D, 0, 
            framebuffer_internal_format,
            @bitCast(gl.GLsizei, framebuffer_width), @bitCast(gl.GLsizei, framebuffer_height), 0,
            gl.RGBA, framebuffer_pixel_type, framebuffer_data.ptr);
    }

    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, framebuffer_tex);

    gl.useProgram(quad_program);
    gl.bindVertexArray(quad_vao);
    gl.drawElements(gl.TRIANGLES, quad_indices.len, gl.UNSIGNED_SHORT, null);
}
