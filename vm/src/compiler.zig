const std = @import("std");
const print = std.debug.print;

usingnamespace @import("common.zig");
usingnamespace @import("value.zig");
usingnamespace @import("chunk.zig");
usingnamespace @import("scanner.zig");
usingnamespace @import("parser.zig");
usingnamespace @import("vm.zig");

pub const Compiler = struct {
    vm: *VM,
    chunk: *Chunk,

    pub fn init(
        vm: *VM,
        chunk: *Chunk,
    ) Compiler {
        return Compiler{
            .chunk = chunk,
            .vm = vm,
        };
    }

    pub fn compile(self: *Compiler, source: []const u8) !bool {
        var scanner = Scanner.init(source);
        var parser = Parser.init(self, &scanner);

        parser.advance();
        try parser.expression();
        parser.consume(.TOKEN_EOF, "Expect end of expression");

        try parser.endParse();
        return !parser.hadError;
    }
};
