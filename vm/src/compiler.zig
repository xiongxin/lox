const std = @import("std");
const print = std.debug.print;

usingnamespace @import("common.zig");
usingnamespace @import("value.zig");
usingnamespace @import("chunk.zig");
usingnamespace @import("scanner.zig");
usingnamespace @import("parser.zig");
usingnamespace @import("vm.zig");

pub const Local = struct {
    name: Token,
    depth: i32,

    pub fn init(name: Token, depth: i32) Local {
        return Local{
            .name = name,
            .depth = depth,
        };
    }
};

pub const Compiler = struct {
    vm: *VM,
    chunk: *Chunk,

    locals: ArrayListOfLocal,
    scopeDepth: i32,

    pub fn init(
        vm: *VM,
        chunk: *Chunk,
    ) Compiler {
        return Compiler{
            .chunk = chunk,
            .vm = vm,
            .locals = ArrayListOfLocal.init(vm.allocator),
            .scopeDepth = 0,
        };
    }

    pub fn addLocal(self: *Compiler, name: Token) !void {
        const local = Local.init(name, -1);
        try self.locals.append(local);
    }
};
