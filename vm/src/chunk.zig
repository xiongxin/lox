const std = @import("std");
const print = std.debug.print;

usingnamespace @import("common.zig");
usingnamespace @import("value.zig");

pub const OpCode = enum(u8) {
    OP_CONSTANT,
    OP_NEGATE,
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_RETURN,
};

pub const Chunk = struct {
    code: ArrayListOfU8,
    constants: ArrayListOfValue,
    lines: ArrayListOfUsize,

    pub fn init(allocator: *std.mem.Allocator) Chunk {
        return Chunk{
            .code = ArrayListOfU8.init(allocator),
            .constants = ArrayListOfValue.init(allocator),
            .lines = ArrayListOfUsize.init(allocator),
        };
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit();
        self.constants.deinit();
        self.lines.deinit();
    }

    pub fn writeByte(self: *Chunk, byte: u8, line: usize) !void {
        try self.code.append(byte);
        try self.lines.append(line);
    }

    pub fn writeCode(self: *Chunk, code: OpCode, line: usize) !void {
        try self.writeByte(@enumToInt(code), line);
    }

    pub fn addConstant(self: *Chunk, value: Value) !u8 {
        try self.constants.append(value);
        return @intCast(u8, self.constants.items.len - 1);
    }

    pub fn disassemble(self: *Chunk, name: []const u8) void {
        print("{:=^50}\n", .{name});

        var offset: usize = 0;
        while (offset < self.code.items.len) {
            offset = self.disassembleInstruction(offset);
        }

        print("\n", .{});
    }

    pub fn disassembleInstruction(self: *Chunk, offset: usize) usize {
        print("{:0>4} ", .{offset});

        if (offset > 0 and self.lines.items[offset] == self.lines.items[offset - 1]) {
            print("   | ", .{});
        } else {
            print("{:0>4} ", .{self.lines.items[offset]});
        }

        const instruction = @intToEnum(OpCode, self.code.items[offset]);
        switch (instruction) {
            .OP_RETURN,
            .OP_NEGATE,
            .OP_ADD,
            .OP_SUBTRACT,
            .OP_MULTIPLY,
            .OP_DIVIDE,
            => return simpleInstruction(@tagName(instruction), offset),
            .OP_CONSTANT => return self.constantInstruction(@tagName(instruction), offset),
        }
    }

    fn simpleInstruction(name: []const u8, offset: usize) usize {
        print("{:<16}\n", .{name});
        return offset + 1;
    }

    fn constantInstruction(self: *Chunk, name: []const u8, offset: usize) usize {
        const constant = self.code.items[offset + 1];
        print("{:<16} {} '", .{ name, constant });
        printValue(self.constants.items[constant]);
        print("'\n", .{});
        return offset + 2;
    }
};
