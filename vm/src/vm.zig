const std = @import("std");
const print = std.debug.print;

usingnamespace @import("common.zig");
usingnamespace @import("value.zig");
usingnamespace @import("chunk.zig");

pub const InterpretResult = enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
};

const STACK_MAX = 256;


pub const VM = struct {
    chunk: *Chunk,
    ip: [*]u8, //instruction pointer

    stack: [STACK_MAX]Value,
    stackTop: [*]Value,

    allocator: *std.mem.Allocator,

    pub fn init(allocator: *std.mem.Allocator, chunk: *Chunk) VM {
        return VM {
            .chunk = chunk,
            .ip = chunk.code.items.ptr,
            .stack = undefined,
            .stackTop = undefined,
            .allocator = allocator,
        };
    }

    pub fn restStack(self: *VM) void {
        self.stackTop = &self.stack;
    }

    pub fn deinit(self: *VM) void {

    }

    pub fn interpret(self: *VM) InterpretResult {
        return self.run();
    }

    fn push(self: *VM, value: Value) void {
        self.stackTop[0] = value;
        self.stackTop += 1;
    }

    fn pop(self: *VM) Value {
        self.stackTop -= 1;
        return self.stackTop[0];
    }

    fn run(self: *VM) InterpretResult {
        while (true) {

            if (DEBUG_TRACE_EXECUTION) {

                print("          ", .{});
                var slot : [*]Value = &self.stack;
                while (@ptrToInt(slot) < @ptrToInt(self.stackTop)) {
                    print("[ ", .{});
                    printValue(slot[0]);
                    print(" ]", .{});
                    slot += 1;
                }
                print("\n", .{});

                _ = self.chunk.disassembleInstruction(
                    @ptrToInt(self.ip) - @ptrToInt(self.chunk.code.items.ptr));
            }

            const instruction = @intToEnum(OpCode, self.readByte());
            switch (instruction) {
                .OP_CONSTANT => {
                    const constant = self.readConstant();
                    self.push(constant);
                },
                .OP_ADD, .OP_SUBTRACT, .OP_MULTIPLY, .OP_DIVIDE, => self.binaryOp(instruction),
                .OP_NEGATE => {
                    self.push(-self.pop());
                },
                .OP_RETURN => {
                    printValue(self.pop());
                    print("\n", .{});
                    return .INTERPRET_OK;
                }
            }
        }
    }

    fn binaryOp(self: *VM, op: OpCode) void {
        const b = self.pop();
        const a = self.pop();

        switch (op) {
            .OP_ADD => self.push(a + b), 
            .OP_SUBTRACT => self.push(a - b), 
            .OP_MULTIPLY, => self.push( a * b),
            .OP_DIVIDE => self.push(a / b),
            else => unreachable,
        }
    }   

    fn readConstant(self: *VM) Value {
        const index = self.readByte();
        return self.chunk.constants.items[index];
    }

    fn readByte(self: *VM) u8 {
        var res = self.ip[0];
        self.ip += 1;
        return res;
    }
};