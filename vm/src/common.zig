const std = @import("std");

usingnamespace @import("value.zig");
usingnamespace @import("vm.zig");
usingnamespace @import("compiler.zig");

pub const ArrayListOfU8 = std.ArrayList(u8);
pub const ArrayListOfValue = std.ArrayList(Value);
pub const ArrayListOfUsize = std.ArrayList(usize);
pub const ArrayListOfLocal = std.ArrayList(Local);
pub const String2OjStringMap = std.StringHashMap(*ObjString);
pub const ObjString2Value = std.AutoHashMap(*ObjString, Value);
pub const DEBUG_TRACE_EXECUTION = true;
pub const DEBUG_PRINT_CODE = true;

pub const Heap = struct {
    allocator: *std.mem.Allocator,
    vm: *VM,

    pub fn init(vm: *VM) Heap {
        return Heap{
            .allocator = vm.allocator,
            .vm = vm,
        };
    }

    pub fn copyString(self: *Heap, chars: []const u8) !*ObjString {
        if (self.vm.strings.get(chars)) |v| {
            return v;
        }

        const heapChars = try self.allocator.alloc(u8, chars.len);
        std.mem.copy(u8, heapChars, chars);

        const res = try self.allocateString(heapChars);
        try self.vm.strings.put(heapChars, res);

        return res;
    }

    pub fn takeString(self: *Heap, chars: []u8) !*ObjString {
        if (self.vm.strings.get(chars)) |v| {
            self.allocator.free(chars);
            return v;
        }

        return self.allocateString(chars);
    }

    fn allocateString(self: *Heap, chars: []u8) !*ObjString {
        var string = try self.allocator.create(ObjString);
        string.chars = chars;
        string.obj = Obj{
            .objType = .OBJ_STRING,
            .next = null,
        };

        var obj = @ptrCast(*Obj, string);
        obj.next = self.vm.objects;
        self.vm.objects = obj;

        return string;
    }
};
