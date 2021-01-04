const std = @import("std");

usingnamespace @import("chunk.zig");

pub fn main() anyerror!void {

    var memory: [1024 * 1024]u8 = undefined;
    var arena = std.heap.ArenaAllocator.init(&std.heap.FixedBufferAllocator.init(memory[0..]).allocator);
    defer arena.deinit();

    var chunk = Chunk.init(&arena.allocator);

    var constant = try chunk.addConstant(1.2);
    try chunk.writeCode(.OP_CONSTANT);
    try chunk.writeByte(constant);

    try chunk.writeCode(.OP_RETURN);
    chunk.disassemble("test chunk");
    defer chunk.deinit();
}
