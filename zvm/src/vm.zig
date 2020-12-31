const std = @import("std");
const print = std.debug.print;

usingnamespace @import("value.zig");
usingnamespace @import("chunk.zig");
usingnamespace @import("compiler.zig");
usingnamespace @import("heap.zig");

pub var vm: VM = undefined;

pub const ObjStringHashOfValue = std.hash_map.AutoHashMap(*ObjString, Value);

pub const ObjStringSet = std.hash_map.StringHashMap(*ObjString);

const CallFrame = struct {
    closure: *ObjClosure,
    ip: usize,
    slots: []Value,

    lastStackTop: usize,

    fn readByte(self: *CallFrame) u8 {
        const instrction = self.closure.fun.chunk.code.items[self.ip];
        self.ip += 1;
        return instrction;
    }

    fn readShort(self: *CallFrame) u16 {
        var offset = @intCast(u16, self.closure.fun.chunk.code.items[self.ip]) << 8;
        offset |= self.closure.fun.chunk.code.items[self.ip + 1];
        self.ip += 2;
        return offset;
    }

    fn readConstant(self: *CallFrame) Value {
        const instrction = self.readByte();
        return self.closure.fun.chunk.constants.items[instrction];
    }

    fn readString(self: *CallFrame) *ObjString {
        const value = self.readConstant();
        return asString(value);
    }
};

const FRAMES_MAX = 64;
const STACK_MAX = std.math.maxInt(u8) * FRAMES_MAX;

pub const VM = struct {

    frames: [FRAMES_MAX]CallFrame,
    frameCount: usize,

    openUpvalue: ?*ObjUpvalue,
    // vm运行时创建的对象
    objects: ?*Obj,
    strings: ObjStringSet,
    stack: [STACK_MAX]Value,
    stackTop: usize,
    globals: ObjStringHashOfValue,

    pub fn init(self: *VM, allocator: *std.mem.Allocator) void {
        self.globals = ObjStringHashOfValue.init(allocator);
        self.strings = ObjStringSet.init(allocator);
        self.restStack();
    }

    pub fn deinit(self: *VM) void {
        self.globals.deinit();
        heap.freeObjects(self.objects);
    }

    fn restStack(self: *VM) void {
        self.stackTop = 0;
        self.frameCount = 0;
        self.openUpvalue = null;
    }

    fn run(self: *VM) !InterpretResult {
        const start = std.time.milliTimestamp();
        var frame = &self.frames[self.frameCount - 1];

        while (true) {
            // 打印栈数据
            print("          ", .{});
            for (self.stack[0 .. self.stackTop]) |item| {
                print("[ ", .{});
                try printValue(item);
                print(" ]", .{});
            }
            print("\n", .{});
            _ = try frame.closure.fun.chunk.disassembleInstruction(frame.ip);

            const instrction = @intToEnum(OpCode, frame.readByte());
            switch (instrction) {
                OpCode.OP_PRINT => {
                    try printValue(self.pop());
                    print("\n", .{});
                },
                OpCode.OP_RETURN => {
                    const result = self.pop();
                    self.closeUpvalues(&self.stack[frame.lastStackTop]);
                    vm.frameCount -= 1;
                    if (vm.frameCount == 0) {
                        const i: f64 = 1000.0;
                        print("执行花费时间: {d:.5}秒\n", .{ std.math.divFloor(f64, @intToFloat(f64, (std.time.milliTimestamp() - start)), i) });
                        return InterpretResult.INTERPRET_OK;
                    }                
                    vm.stackTop = frame.lastStackTop;
                    self.push(result);
                    frame = &vm.frames[vm.frameCount - 1];
                },
                OpCode.OP_CONSTANT => {
                    const constant = frame.readConstant();
                    self.push(constant);
                },
                OpCode.OP_NIL   => self.push(nil2Value()),
                OpCode.OP_TRUE  => self.push(bool2Value(true)),
                OpCode.OP_FALSE => self.push(bool2Value(false)),
                OpCode.OP_NOT   => self.push(bool2Value(isFalsey(self.pop()))),
                OpCode.OP_POP   => _ = self.pop(),
                OpCode.OP_GET_LOCAL     => {
                    const slot = frame.readByte();
                    self.push(frame.slots[slot]);
                },
                OpCode.OP_SET_LOCAL     => {
                    const slot = frame.readByte();
                    frame.slots[slot] = self.peek(0);
                },
                OpCode.OP_GET_GLOBAL    => {
                    const name = frame.readString();
                    if (self.globals.get(name)) |value| {
                        self.push(value);
                    } else {
                        self.runtimeError("Undefined variable '{}'.", .{name});
                        return InterpretResult.INTERPRET_RUNTIME_ERROR;
                    }
                },
                OpCode.OP_SET_GLOBAL    => {
                    const name = frame.readString();
                    if (self.globals.get(name)) |_| {
                        try self.globals.put(name, self.peek(0));
                    } else {
                        self.runtimeError("Undefined variable '{}'.", .{name.chars});
                        return InterpretResult.INTERPRET_RUNTIME_ERROR;
                    }
                },
                OpCode.OP_DEFINE_GLOBAL => {
                    const name = frame.readString();
                    try self.globals.put(name, self.peek(0));
                    _ = self.pop();
                },
                OpCode.OP_EQUAL => {
                    const b = self.pop();
                    const a = self.pop();
                    self.push(bool2Value(valuesEqual(a, b)));
                },
                OpCode.OP_ADD, OpCode.OP_SUBTRACT, OpCode.OP_MULTIPLY, OpCode.OP_DIVIDE,
                OpCode.OP_GREATER, OpCode.OP_LESS,
                => {
                    if (!isNumber(self.peek(0)) or !isNumber(self.peek(1))) {
                        self.runtimeError("Operands must be numbers.", .{});
                        return InterpretResult.INTERPRET_RUNTIME_ERROR;
                    }
                    self.binaryOp(instrction);
                },
                OpCode.OP_NEGATE => {
                    if (!isNumber(self.peek(0))) {
                        self.runtimeError("Operand must be a number.", .{});
                        return InterpretResult.INTERPRET_RUNTIME_ERROR;
                    }

                    self.push(number2Value(-asNumber(self.pop())));
                },
                OpCode.OP_JUMP_IF_FALSE => {
                    const offset = frame.readShort();
                    if (isFalsey(self.peek(0))) 
                        frame.ip += offset;
                },
                OpCode.OP_JUMP => {
                    const offset = frame.readShort();
                    frame.ip += offset;
                },
                OpCode.OP_LOOP => {
                    const offset = frame.readShort();
                    frame.ip -= offset;
                },
                OpCode.OP_CALL => {
                    const argCount = frame.readByte();
                    if (!self.callValue(self.peek(argCount), argCount)) {
                        return InterpretResult.INTERPRET_RUNTIME_ERROR;
                    }
                    frame = &self.frames[vm.frameCount - 1];
                },
                OpCode.OP_CLOSURE => {
                    const fun = asFunction(frame.readConstant());
                    const closure = try heap.newClosure(fun);
                    self.push(objClosure2Value(closure));

                    var i : usize = 0;
                    while(i < closure.fun.upvalueCount) : (i += 1) {
                        const isLocal = frame.readByte();
                        const index   = frame.readByte();
                        if (isLocal == 1) {
                            try closure.upvalues.append((try self.captureUpvalue(&frame.slots[index])));
                        } else {
                            try closure.upvalues.append(frame.closure.upvalues.items[index]);
                        }
                    }
                },
                OpCode.OP_SET_UPVALUE => {
                    const slot = frame.readByte();
                    frame.closure.upvalues.items[slot].location.* = self.peek(0);
                },
                OpCode.OP_GET_UPVALUE => {
                    const slot = frame.readByte();
                    self.push(frame.closure.upvalues.items[slot].location.*);
                },
                OpCode.OP_CLOSE_UPVALUE => {
                    //self.closeUpvalues(&self.peek(0));
                }
            }
        }
    }

    fn closeUpvalues(self: *VM, last: *Value) void {
        while (self.openUpvalue != null and @ptrToInt(self.openUpvalue.?.location) >= @ptrToInt(last)) {
            var upvalue = self.openUpvalue.?;
            upvalue.closed = upvalue.location.*;
            upvalue.location = &upvalue.closed;
            self.openUpvalue = upvalue.next;
        }
    }

    fn captureUpvalue(self: *VM, local: *Value) !*ObjUpvalue {
        var preUpvalue: ?*ObjUpvalue = null;
        var upvalue = vm.openUpvalue;

        while ( upvalue != null and @ptrToInt(upvalue.?.location) > @ptrToInt(local) ) {
            preUpvalue = upvalue;
            upvalue    = upvalue.?.next;
        }

        if (upvalue != null and upvalue.?.location == local) {
            return upvalue.?;
        }

        var createdUpvalue = try heap.newUpvalue(local);
        createdUpvalue.next = upvalue;

        if (preUpvalue == null) {
            vm.openUpvalue = createdUpvalue;
        } else {
            preUpvalue.?.next = createdUpvalue;
        }

        return createdUpvalue;
    }

    fn callValue(self: *VM, callee: Value, argCount: u8) bool {
        if (isClosure(callee)) {
            return self.call(asClosure(callee), argCount);
        }

        self.runtimeError("Can only call functions and classes.", .{});
        return false;
    }

    fn call(self: *VM, closure: *ObjClosure, argCount: u8) bool {

        if (argCount != closure.fun.arity) {
            self.runtimeError("Expected {} arguments but got {}.",
                        .{ closure.fun.arity, argCount });
            return false;
        }

        if (vm.frameCount == FRAMES_MAX) {
            self.runtimeError("Stack overflow.", .{});
            return false;
        }

        var frame = &vm.frames[vm.frameCount];
        frame.closure = closure;
        frame.ip = 0;
        frame.slots = self.stack[self.stackTop - argCount - 1 .. ];
        frame.lastStackTop = self.stackTop  - argCount - 1;

        vm.frameCount += 1;

        return true;
    }

    fn binaryOp(self: *VM, opCode: OpCode) void {
        const b = asNumber(self.pop());
        const a = asNumber(self.pop());

        switch (opCode) {
            OpCode.OP_ADD => self.push(number2Value(a + b)),
            OpCode.OP_SUBTRACT => self.push(number2Value(a - b)),
            OpCode.OP_MULTIPLY => self.push(number2Value(a * b)),
            OpCode.OP_DIVIDE => self.push(number2Value(a / b)),
            OpCode.OP_GREATER => self.push(bool2Value(a > b)),
            OpCode.OP_LESS    => self.push(bool2Value(a < b)),   
            else => unreachable,         
        }
    }


    fn push(self: *VM, value: Value) void {
        self.stack[self.stackTop] = value;
        self.stackTop += 1;
    }

    fn pop(self: *VM) Value {
        self.stackTop -= 1;
        return self.stack[self.stackTop];
    }

    fn peek(self: *VM, distance: usize) Value {
        return self.stack[self.stackTop - 1 - distance];
    }

    fn runtimeError(self: *VM, comptime msg: []const u8, format: anytype) void {
        print(msg, format);
        print("\n", .{});

        var i = vm.frameCount - 1;
        while (i >= 0) {
            const frame = vm.frames[i];
            const function = frame.closure.fun;
            const instruction = frame.ip;
            print("[line {}]", .{function.chunk.lines.items[instruction]});
            if (function.name) |name| {
                print("{}()\n", .{name.chars});
            } else {
                print("script\n", .{});
            }

            if (@subWithOverflow(usize, i, 1, &i)) {
                break;
            }
        }

        self.restStack();
    }
};


pub const InterpretResult = enum(u2) {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
};

pub fn interpret(allocator: *std.mem.Allocator, source: []const u8) !InterpretResult {
    heap = Heap.init(allocator);

    const function = try compile(allocator, source);

    if (function) |fun| {
        vm.push(objFunction2Value(fun));
        const closure = try heap.newClosure(fun);
        _ = vm.pop();
        vm.push(objClosure2Value(closure));
        _ = vm.callValue(objClosure2Value(closure), 0);

        return try vm.run();
    } else {
        return InterpretResult.INTERPRET_COMPILE_ERROR;
    }

    
}