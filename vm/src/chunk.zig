const std = @import("std");
const print = std.debug.print;

usingnamespace @import("common.zig");
usingnamespace @import("value.zig");

pub const OpCode = enum(u8) {
    OP_CONSTANT,
    OP_RETURN,
};

pub const Chunk = struct {
    code: ArrayListOfU8,
    constants: ArrayListOfValue,

    pub fn init(allocator: *std.mem.Allocator) Chunk {
        return Chunk {
            .code = ArrayListOfU8.init(allocator),
            .constants = ArrayListOfValue.init(allocator),
        };
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit();
        self.constants.deinit();
    }

    pub fn writeByte(self: *Chunk, byte: u8) !void {
        try self.code.append(byte);
    }

    pub fn writeCode(self: *Chunk, code: OpCode) !void {
        try self.writeByte(@enumToInt(code));
    }

    pub fn addConstant(self: *Chunk, value: Value) !u8 {
        try self.constants.append(value);
        return @intCast(u8, self.constants.items.len - 1);
    }

    pub fn disassemble(self: *Chunk, name: []const u8) void {
        print("{:=^50}\n", .{name});

        var offset : usize = 0;
        while (offset < self.code.items.len) {
            offset = self.disassembleInstruction(offset);
        }
    }

    pub fn disassembleInstruction(self: *Chunk, offset: usize) usize {
        print("{:0>4} ", .{ offset });

        const instruction = @intToEnum(OpCode, self.code.items[offset]);
        switch (instruction) {
            .OP_RETURN => return simpleInstruction(@tagName(instruction), offset),
            .OP_CONSTANT => return self.constantInstruction(@tagName(instruction), offset)
        }
    }

    fn simpleInstruction(name: []const u8, offset: usize) usize {
        print("{:<16}\n", .{ name });
        return offset + 1;
    }

    fn constantInstruction(self: *Chunk, name: []const u8, offset: usize) usize {
        const constant = self.code.items[offset];
        print("{:<16} {:0>4} '", .{ name, constant});
        printValue(&self.constants.items[constant]);
        print("'\n", .{});
        return offset + 2;
    }
};