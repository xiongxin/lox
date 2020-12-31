const std = @import("std");
const print = std.debug.print;

usingnamespace @import("value.zig");
usingnamespace @import("chunk.zig");
usingnamespace @import("scanner.zig");
usingnamespace @import("parser.zig");
usingnamespace @import("gc.zig");
usingnamespace @import("vm.zig");

pub var heap: Heap = undefined;

pub const Heap = struct {

    allocator: *std.mem.Allocator,

    pub fn init(allocator: *std.mem.Allocator) Heap {
        return Heap {
            .allocator = allocator,
        };
    }

    fn create(self: *Heap, comptime T: type) !*T {
        try collectGarage();
        const res = try self.allocator.create(T);
        GCLOG.info("{} allocate {} for {}", .{@ptrToInt(res), @sizeOf(T), @typeName(T)});
        return res;
    }

    fn destory(self: *Heap, ptr: anytype) void {
        GCLOG.info("{} free type {}", .{@ptrToInt(ptr), @tagName((ptr.objType))});

        switch (ptr.objType) {
            .str => {
                const str = @ptrCast(*ObjString, ptr);
                self.allocator.free(str.chars);
                self.allocator.destroy(str);
            },
            .fun => self.allocator.destroy(@ptrCast(*ObjFunction, ptr)),
            .closure => self.allocator.destroy(@ptrCast(*ObjClosure, ptr)),
            .upvalue => self.allocator.destroy(@ptrCast(*ObjUpvalue, ptr)),
        }
    }

    pub fn freeObjects(self: *Heap, objects: ?*Obj) void {
        if (objects) |obj| {
            var next = obj.next;
            const first = obj;

            while (next) |on| {
                next = on.next;
                self.destory(on);
            }
            
            self.destory(first);
        }
    }

    fn alloc(self: *Heap, comptime T: type, n: usize) !*T {
        //try collectGarage();
        return try self.allocator.alloc(T, n);
    }

    fn setVmObjects(obj: *Obj) void {
        obj.next = vm.objects;
        vm.objects = obj;
    }

    pub fn copyString(self: *Heap, chars: []const u8) !*ObjString {
        if (vm.strings.get(chars)) |val| {
            return val;
        } else {
            var str_array = try self.allocator.alloc(u8, chars.len);
            std.mem.copy(u8, str_array, chars);

            var objString = try self.create(ObjString);
            objString.obj = .{ .objType = .str, .isMarked = false, .next = null };
            objString.chars = str_array;

            setVmObjects(@ptrCast(*Obj, objString));
            
            try vm.strings.put(chars, objString);

            return objString;
        }
    }

    pub fn newFunction(self: *Heap) !*ObjFunction {

        var function = try self.create(ObjFunction);
        function.obj = .{ .objType = .fun, .isMarked = false, .next = null };
        function.arity = 0;
        function.name  = null;
        function.chunk = try Chunk.init(self.allocator);
        function.upvalueCount = 0;

        setVmObjects(@ptrCast(*Obj, function));

        return function;
    }

    pub fn newClosure(self: *Heap, fun: *ObjFunction) !*ObjClosure {
        var closure = try self.create(ObjClosure);
        closure.obj = .{ .objType = .closure, .isMarked = false, .next = null };
        closure.fun = fun;
        closure.upvalues = ArrayListOfObjUpvalue.init(self.allocator);

        setVmObjects(@ptrCast(*Obj, closure));

        return closure;
    }

    pub fn newUpvalue(self: *Heap, slot: *Value) !*ObjUpvalue  {
        var upvalue = try self.create(ObjUpvalue);
        upvalue.obj = .{ .objType = .upvalue, .isMarked = false, .next = null };
        upvalue.location = slot;
        upvalue.next = null;
        upvalue.closed = Value.nil;

        setVmObjects(@ptrCast(*Obj, upvalue));

        return upvalue;
    }
};