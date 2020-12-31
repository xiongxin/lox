const std = @import("std");
const print = std.debug.print;

usingnamespace @import("value.zig");

pub const OpCode = enum(u8) {
    OP_CONSTANT,
    OP_NIL,
    OP_TRUE,
    OP_FALSE,
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_NOT,
    OP_NEGATE,
    OP_PRINT,
    OP_JUMP,
    OP_JUMP_IF_FALSE,
    OP_LOOP,
    OP_CALL,
    OP_CLOSURE,
    OP_GET_UPVALUE,
    OP_SET_UPVALUE,
    OP_CLOSE_UPVALUE,
    OP_RETURN,
    OP_POP,
    OP_GET_LOCAL,
    OP_SET_LOCAL,
    OP_GET_GLOBAL,
    OP_DEFINE_GLOBAL,
    OP_SET_GLOBAL,
    OP_EQUAL,
    OP_GREATER,
    OP_LESS,
};


pub const ArrayListOfU8 = std.ArrayList(u8);
pub const ArrayListOfUsize = std.ArrayList(usize);

pub const Chunk = struct {
    code: ArrayListOfU8,
    constants: ArrayListOfValue,
    lines: ArrayListOfUsize,
    allocator: *std.mem.Allocator,

    pub fn init(allocator: *std.mem.Allocator) !*Chunk {
        var chunk = try allocator.create(Chunk);
        chunk.code = ArrayListOfU8.init(allocator);
        chunk.constants = ArrayListOfValue.init(allocator);
        chunk.lines = ArrayListOfUsize.init(allocator);
        chunk.allocator = allocator;
        return chunk;
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit();
        self.constants.deinit();
        self.lines.deinit();
        self.allocator.destroy(self);
    }

    pub fn writeByte(self: *Chunk, byte: usize, line: usize) !void {
        try self.code.append(@intCast(u8, byte));
        try self.lines.append(line);
    }

    pub fn writeOpCode(self: *Chunk, opCode: OpCode, line: usize) !void {
        try self.writeByte(@enumToInt(opCode), line);
    }

    pub fn addConstant(self: *Chunk, value: Value) !usize {
        try self.constants.append(value);
        return self.constants.items.len - 1;
    }

    pub fn disassembleChunk(self: *Chunk, name: []const u8) !void {
        print("== {} ==\n", .{name});
        var offset: usize = 0;
        while(offset < self.code.items.len) {
            offset = try self.disassembleInstruction(offset);
        }
        print("== {} ==\n", .{name});
    }

    pub fn disassembleInstruction(self: *Chunk, offset: usize) !usize {
        print("{:0>4} ", .{offset});

        if (offset > 0 and self.lines.items[offset] == self.lines.items[offset - 1]) {
            print("   | ", .{});
        } else {
            print("{:4} ", .{self.lines.items[offset]});
        }

        const instruction = @intToEnum(OpCode, self.code.items[offset]);
        return switch (instruction) {
            OpCode.OP_RETURN,OpCode.OP_NEGATE, OpCode.OP_ADD, OpCode.OP_SUBTRACT, OpCode.OP_MULTIPLY,
            OpCode.OP_DIVIDE, OpCode.OP_NIL, OpCode.OP_TRUE, OpCode.OP_FALSE, OpCode.OP_NOT,
            OpCode.OP_GREATER, OpCode.OP_LESS, OpCode.OP_EQUAL, OpCode.OP_PRINT, OpCode.OP_POP,
            OpCode.OP_CLOSE_UPVALUE,
                => simpleInstruction(@tagName(instruction), offset),
            OpCode.OP_CONSTANT, OpCode.OP_DEFINE_GLOBAL, OpCode.OP_GET_GLOBAL, OpCode.OP_SET_GLOBAL,
            
                => try self.constantInstruction(@tagName(instruction), offset),
            OpCode.OP_GET_LOCAL, OpCode.OP_SET_LOCAL, OpCode.OP_CALL,OpCode.OP_GET_UPVALUE, OpCode.OP_SET_UPVALUE,
                => self.byteInstruction(@tagName(instruction), offset),
            OpCode.OP_JUMP_IF_FALSE, OpCode.OP_JUMP
                => self.jumInstruction(@tagName(instruction), 1, offset),
            OpCode.OP_LOOP 
                => self.jumInstruction(@tagName(instruction), -1, offset),
            OpCode.OP_CLOSURE => {
                var res = offset + 1;
                const constant = self.code.items[res];
                res += 1;
                print("{s:<16} {:>4} ", .{@tagName(instruction), constant});
                try printValue(self.constants.items[constant]);
                print("\n", .{});

                const function = asFunction(self.constants.items[constant]);
                var j: usize = 0;
                while(j < function.upvalueCount) : (j += 1) {
                    const isLocal = self.code.items[res];
                    res += 1;
                    const index = self.code.items[res];
                    res += 1;
                    const local: []const u8 = if (isLocal == 1) "local" else "upvalue";
                    print("{:>4}      |                     {} {}\n", 
                        .{ res - 2, local, index});
                }

                return res;
            }
        };
    }

    fn simpleInstruction(name: []const u8, offset: usize) usize {
        print("{s:<16}\n", .{name});
        return offset + 1;
    }


    fn constantInstruction(self: *Chunk, name: []const u8, offset: usize) !usize {
        const contantIndex = self.code.items[offset + 1];
        print("{s:<16} {:>4}(", .{name, contantIndex});
        try printValue(self.constants.items[contantIndex]);
        print(")\n", .{});
        return offset + 2;
    }

    fn byteInstruction(self: *Chunk, name: []const u8, offset: usize) usize {
        const slot = self.code.items[offset + 1];
        print("{s:<16} {:>4}\n", .{name, slot});
        return offset + 2;
    }

    fn jumInstruction(self: *Chunk,name: []const u8, sign: i32, offset: usize) usize {
        var jump = @intCast(u16, self.code.items[offset + 1]) << 8;
        jump |= self.code.items[offset + 2];
        print("{s:<16} {:>4} -> {}\n", .{name, offset, @intCast(i32, offset) + 3 + @intCast(i32, sign) * @intCast(i32, jump)});
        return offset + 3;
    }
};

pub fn printValue(value: Value) anyerror!void {
    switch (value) {
        .nil     => print("nil", .{}),
        .boolean => |boolean| print("{}", .{boolean}),
        .number  => |number| print("{d:.5}", .{number}),
        .obj     => |obj| {
            switch (obj.objType) {
                .str => print("'{}'", .{ @ptrCast(*ObjString, obj).chars }),
                .fun => {
                    const fun = @ptrCast(*ObjFunction, obj);
                    if (fun.name) |name| {
                        print("<fn {}>", .{name.chars});
                    } else {
                        print("<script>", .{});
                    }
                },
                .closure => {
                    try printValue(objFunction2Value(@ptrCast(*ObjClosure, obj).fun));
                },
                .upvalue => print("upvalue", .{}),
            }
        },
        
    }
    
}