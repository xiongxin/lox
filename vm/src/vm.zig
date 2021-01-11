const std = @import("std");
const print = std.debug.print;

usingnamespace @import("common.zig");
usingnamespace @import("value.zig");
usingnamespace @import("chunk.zig");
usingnamespace @import("compiler.zig");

pub const InterpretResult = enum {
    INTERPRET_OK, INTERPRET_COMPILE_ERROR, INTERPRET_RUNTIME_ERROR
};

const STACK_MAX = 256;

pub const VM = struct {
    chunk: *Chunk,
    ip: [*]u8, //instruction pointer

    stack: [STACK_MAX]Value,
    stackTop: [*]Value,

    allocator: *std.mem.Allocator,

    pub fn init(allocator: *std.mem.Allocator) VM {
        return VM{
            .chunk = undefined,
            .ip = undefined, //chunk.code.items.ptr,
            .stack = undefined,
            .stackTop = undefined,
            .allocator = allocator,
        };
    }

    pub fn restStack(self: *VM) void {
        self.stackTop = &self.stack;
    }

    pub fn deinit(self: *VM) void {}

    pub fn interpret(self: *VM, source: []const u8) !InterpretResult {
        var chunk = Chunk.init(self.allocator);
        defer chunk.deinit();
        if (!(try self.compile(source, &chunk))) {
            return .INTERPRET_RUNTIME_ERROR;
        }

        self.chunk = &chunk;
        self.ip = self.chunk.code.items.ptr;

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

    fn peek(self: *VM, distance: usize) Value {
        const value = self.stackTop - 1 - distance;
        return value[0];
    }

    fn runtimeError(self: *VM, comptime format: []const u8, args: anytype) void {
        print(format, args);
        print("\n", .{});
        const instruction = @ptrToInt(self.ip) - @ptrToInt(self.chunk.code.items.ptr) - 1;
        const line = self.chunk.lines.items[instruction];
        print("[line {}] in script\n", .{line});

        self.restStack();
    }

    fn run(self: *VM) InterpretResult {
        while (true) {
            if (DEBUG_TRACE_EXECUTION) {
                print("          ", .{});
                var slot: [*]Value = &self.stack;
                while (@ptrToInt(slot) < @ptrToInt(self.stackTop)) {
                    print("[ ", .{});
                    printValue(slot[0]);
                    print(" ]", .{});
                    slot += 1;
                }
                print("\n", .{});

                _ = self.chunk.disassembleInstruction(@ptrToInt(self.ip) - @ptrToInt(self.chunk.code.items.ptr));
            }

            const instruction = @intToEnum(OpCode, self.readByte());
            switch (instruction) {
                .OP_CONSTANT => {
                    const constant = self.readConstant();
                    self.push(constant);
                },
                .OP_NIL => self.push(NIL_VAL()),
                .OP_FALSE => self.push(BOOL_VAL(false)),
                .OP_TRUE => self.push(BOOL_VAL(true)),
                .OP_ADD,
                .OP_SUBTRACT,
                .OP_MULTIPLY,
                .OP_DIVIDE,
                => {
                    if (!IS_NUMBER(self.peek(0)) or !IS_NUMBER(self.peek(1))) {
                        self.runtimeError("Operands must be a numbers", .{});
                        return .INTERPRET_RUNTIME_ERROR;
                    } else {
                        self.binaryOp(instruction);
                    }
                },
                .OP_NOT => {
                    self.push(BOOL_VAL(isFalsey(self.pop())));
                },
                .OP_NEGATE => {
                    if (!IS_NUMBER(self.peek(0))) {
                        self.runtimeError("Operand must be a number", .{});
                        return .INTERPRET_RUNTIME_ERROR;
                    }
                    self.push(NUMBER_VAL(-AS_NUMBER(self.pop())));
                },
                .OP_RETURN => {
                    printValue(self.pop());
                    print("\n", .{});
                    return .INTERPRET_OK;
                },
            }
        }
    }

    fn binaryOp(self: *VM, op: OpCode) void {
        const b = AS_NUMBER(self.pop());
        const a = AS_NUMBER(self.pop());

        switch (op) {
            .OP_ADD => self.push(NUMBER_VAL(a + b)),
            .OP_SUBTRACT => self.push(NUMBER_VAL(a - b)),
            .OP_MULTIPLY => self.push(NUMBER_VAL(a * b)),
            .OP_DIVIDE => self.push(NUMBER_VAL(a / b)),
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

    fn compile(self: *VM, source: []const u8, chunk: *Chunk) !bool {
        var compiler = Compiler.init(self, chunk);
        return try compiler.compile(source);
    }

    fn isFalsey(value: Value) bool {
        return IS_NIL(value) or (IS_BOOL(value) and !AS_BOOL(value));
    }
};
