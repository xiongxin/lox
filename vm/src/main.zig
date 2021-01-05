const std = @import("std");

usingnamespace @import("chunk.zig");
usingnamespace @import("vm.zig");

pub fn main() anyerror!void {

    var memory: [1024 * 1024]u8 = undefined;
    var arena = std.heap.ArenaAllocator.init(&std.heap.FixedBufferAllocator.init(memory[0..]).allocator);
    defer arena.deinit();

    var chunk = Chunk.init(&arena.allocator);
    defer chunk.deinit();

    var constant = try chunk.addConstant(1.2);
    try chunk.writeCode(.OP_CONSTANT, 123);
    try chunk.writeByte(constant, 123);

    constant = try chunk.addConstant(3.4);
    try chunk.writeCode(.OP_CONSTANT, 123);
    try chunk.writeByte(constant, 123);

    try chunk.writeCode(.OP_ADD, 123);

    constant = try chunk.addConstant(5.6);
    try chunk.writeCode(.OP_CONSTANT, 123);
    try chunk.writeByte(constant, 123);

    try chunk.writeCode(.OP_DIVIDE, 123);
    try chunk.writeCode(.OP_NEGATE, 123);

    try chunk.writeCode(.OP_RETURN, 123);
    chunk.disassemble("test chunk");


    var vm = VM.init(&arena.allocator, &chunk);
    defer vm.deinit();
    vm.restStack();

    _ = vm.interpret();
}
