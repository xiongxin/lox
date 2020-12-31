const std = @import("std");
const print = std.debug.print;

usingnamespace @import("value.zig");
usingnamespace @import("chunk.zig");
usingnamespace @import("scanner.zig");
usingnamespace @import("parser.zig");
usingnamespace @import("heap.zig");

pub const ArrayListOfLocal = std.ArrayList(Local);

pub const Local = struct {
    name: Token,
    depth: i32,

    isCaptured: bool,

    fn init(name: Token, depth: i32) Local {
        return Local { .name = name, .depth = depth };
    }
};

const Upvalue = struct {
    index: usize,
    isLocal: bool,
};

pub const FunctionType = enum {
    TYPE_FUNCTION,
    TYPE_SCRIPT
};

const ArrayListOfUpvalue = std.ArrayList(Upvalue);

pub const Compiler = struct {
    function: ?*ObjFunction,
    functionType: FunctionType,
    enclosing: ?*Compiler,
    upvalues: ArrayListOfUpvalue,

    locals: ArrayListOfLocal,
    scopeDepth: i32,
    allocator: *std.mem.Allocator,

    pub fn init(allocator: *std.mem.Allocator) !*Compiler {
        var res = try allocator.create(Compiler);
        res.allocator = allocator;
        res.locals = ArrayListOfLocal.init(allocator);
        res.upvalues = ArrayListOfUpvalue.init(allocator);
        return res;
    }

    pub fn deinit(self: *Compiler) void {
        self.locals.deinit();
        self.upvalues.deinit();
        self.allocator.destroy(self);
    }

    pub fn beginScope(self: *Compiler) void {
        self.scopeDepth += 1;
    }

    pub fn endScope(self: *Compiler) !void {
        self.scopeDepth -= 1;

        while (current.?.locals.items.len > 0 and 
                current.?.locals.items[current.?.locals.items.len - 1].depth > current.?.scopeDepth) {
            if (self.locals.items[self.locals.items.len - 1].isCaptured) {
                try parser.emitOpCode(OpCode.OP_CLOSE_UPVALUE);
            } else {
                try parser.emitOpCode(OpCode.OP_POP);
            }
            _ = current.?.locals.pop();
        }
    }

    pub fn addLocal(self: *Compiler, name: Token) !void {
        try self.locals.append(Local {
            .name = name,
            .depth = -1,
            .isCaptured = false,
        }); // 本地变量默认是未初始化变量
    }

    pub fn resolveLocal(self: *Compiler, name: Token) i32 {
        if (self.locals.items.len == 0) return -1;
        var i: usize = self.locals.items.len - 1;
        while (i >= 0) {
            const local = self.locals.items[i];
            if (Token.equal(name, local.name)) {
                return @intCast(i32, i);
            }

            if (@subWithOverflow(usize, i, 1, &i)) {
                if (local.depth == -1) {
                    parser.error0("Cann't read local variable in its own initializer.");
                }
                break;
            }
        }

        return -1;
    }

    pub fn resolveUpvalue(self: *Compiler, name: Token) anyerror!i32 {
        if (self.enclosing) |enclosing| {
            const local = enclosing.resolveLocal(name);
            if (local != -1) {
                // 该变量需要移动到heap上
                self.enclosing.?.locals.items[@intCast(usize, local)].isCaptured = true;
                return @intCast(i32, try self.addUpvalue(@intCast(usize, local), true));
            }

            const upvalue = try enclosing.resolveUpvalue(name);
            if (upvalue != -1) {
                return @intCast(i32, try self.addUpvalue(@intCast(usize, upvalue), false));
            }
        }
            
        return -1;
    }

    fn addUpvalue(self: *Compiler, index: usize, isLocal: bool) !usize {
        const upvalueCount = self.function.?.upvalueCount;

        var i: usize = 0;
        while ( i < upvalueCount) : (i += 1) {
            const upvalue = self.upvalues.items[i] ;
            if (upvalue.index == index and upvalue.isLocal == isLocal) {
                return i;
            }
        }

        try self.upvalues.append(Upvalue { .index = index, .isLocal = isLocal });
        self.function.?.upvalueCount += 1;
        return upvalueCount;
    }

    pub fn endCompiler(self: *Compiler) !*ObjFunction {
        try parser.emitReturn();

        const function = self.function;
        if (!parser.hadError) {
            if (function.?.name) |name| {
                try currentChunk().disassembleChunk(name.chars);
            } else {
                try currentChunk().disassembleChunk("<script>");
            }
        }
        current = self.enclosing;
        //defer self.deinit();

        return function.?;
    }

    // 标记本地变量初始化
    pub fn makeInitialized(self: *Compiler) void {
        if (self.scopeDepth == 0) return;
        self.locals.items[self.locals.items.len - 1].depth = self.scopeDepth;
    }
};

pub var current: ?*Compiler = null;

pub var compilingChunk: *Chunk = undefined;

pub fn currentChunk() *Chunk {
    return current.?.function.?.chunk;
}

pub fn initCompiler(allocator: *std.mem.Allocator, ft: FunctionType) !void {
    const compiler = try Compiler.init(allocator);
    compiler.enclosing = current;
    compiler.scopeDepth = 0;
    compiler.function = null;
    compiler.functionType = ft;
    compiler.function = try heap.newFunction();

    current = compiler;

    if (ft != FunctionType.TYPE_SCRIPT) {
        current.?.function.?.name = try heap.copyString(parser.previous.literal);
    }

    var local = Local {
        .depth = 0,
        .name  = Token { 
            .tokenType = TokenType.TOKEN_FUN,
            .line = 0,
            .literal = "",
        },
        .isCaptured = false,
    };

    try current.?.locals.append(local);
}

pub fn compile(allocator: *std.mem.Allocator, source: []const u8) !?*ObjFunction {
    Scanner.init(source);

    try initCompiler(allocator, FunctionType.TYPE_SCRIPT);
    
    parser = Parser.init();
    parser.advance();

    while (!parser.match(TokenType.TOKEN_EOF)) {
        try parser.declaration();
    }
    const function = try current.?.endCompiler();
    return if (parser.hadError) null else function;
}