const std = @import("std");
const core = @import("mach").core;
const gpu = core.gpu;
const state = @import("state.zig");
const cartHeader = @import("cartHeader.zig");
const cpu = @import("cpu.zig");

pub const App = @This();

title_timer: core.Timer,
pipeline: *gpu.RenderPipeline,
allocator: std.mem.Allocator,
rom_data: []u8,
header: cartHeader.CartridgeHeader,
st: state.State,

pub fn init(app: *App) !void {
    try core.init(.{});

    const shader_module = core.device.createShaderModuleWGSL("shader.wgsl", @embedFile("shader.wgsl"));
    defer shader_module.release();

    // Fragment state
    const blend = gpu.BlendState{};
    const color_target = gpu.ColorTargetState{
        .format = core.descriptor.format,
        .blend = &blend,
        .write_mask = gpu.ColorWriteMaskFlags.all,
    };
    const fragment = gpu.FragmentState.init(.{
        .module = shader_module,
        .entry_point = "frag_main",
        .targets = &.{color_target},
    });
    const pipeline_descriptor = gpu.RenderPipeline.Descriptor{
        .fragment = &fragment,
        .vertex = gpu.VertexState{
            .module = shader_module,
            .entry_point = "vertex_main",
        },
    };
    const pipeline = core.device.createRenderPipeline(&pipeline_descriptor);

    const allocator = std.heap.page_allocator;
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    const rom_file = try std.fs.cwd().openFile(args[1], .{});
    const rom_data: []u8 = try rom_file.readToEndAlloc(allocator, 8192 * 1024);

    const header: cartHeader.CartridgeHeader = cartHeader.CartridgeHeader.init(rom_data);
    header.pp();

    var st: state.State = state.State.init();
    st.mapMemory(rom_data, header.getMapper());

    app.* = .{ .title_timer = try core.Timer.start(), .pipeline = pipeline, .allocator = allocator, .rom_data = rom_data, .header = header, .st = st };
}

pub fn deinit(app: *App) void {
    defer core.deinit();
    defer app.allocator.free(app.rom_data);
}

pub fn update(app: *App) !bool {
    var iter = core.pollEvents();
    while (iter.next()) |event| {
        switch (event) {
            .close => return true,
            else => {},
        }
    }

    const queue = core.queue;
    const back_buffer_view = core.swap_chain.getCurrentTextureView().?;
    const color_attachment = gpu.RenderPassColorAttachment{
        .view = back_buffer_view,
        .clear_value = std.mem.zeroes(gpu.Color),
        .load_op = .clear,
        .store_op = .store,
    };

    const encoder = core.device.createCommandEncoder(null);
    const render_pass_info = gpu.RenderPassDescriptor.init(.{
        .color_attachments = &.{color_attachment},
    });
    const pass = encoder.beginRenderPass(&render_pass_info);
    pass.setPipeline(app.pipeline);
    pass.draw(3, 1, 0, 0);
    pass.end();
    pass.release();

    var command = encoder.finish(null);
    encoder.release();

    queue.submit(&[_]*gpu.CommandBuffer{command});
    command.release();
    core.swap_chain.present();
    back_buffer_view.release();

    // update the window title every second
    if (app.title_timer.read() >= 1.0) {
        app.title_timer.reset();
        try core.printTitle("Triangle [ {d}fps ] [ Input {d}hz ]", .{
            core.frameRate(),
            core.inputRate(),
        });
    }

    std.debug.print("\u{001B}[1m@PC=${X}:\u{001B}[0m\t", .{app.st.getReg(state.Regs.PC)});
    cpu.execute(&app.st);
    app.st.pp();

    return false;
}

test "Read ROM" {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();
    const rom_path = "/opt/test_roms/cpu_instrs.gb";
    const allocator = std.heap.page_allocator;
    const rom_file = try std.fs.cwd().openFile(rom_path, .{});
    const rom_data: []u8 = try rom_file.readToEndAlloc(allocator, 8192 * 1024);
    defer allocator.free(rom_data);
    try stdout.print("Read in {s} of size {d}\n", .{ rom_path, rom_data.len });
    try bw.flush();
}
