const std = @import("std");

usingnamespace @import("value.zig");
usingnamespace @import("vm.zig");

pub const ArrayListOfU8 = std.ArrayList(u8);
pub const ArrayListOfValue = std.ArrayList(Value);
pub const ArrayListOfUsize = std.ArrayList(usize);
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
        const heapChars = try self.allocator.alloc(u8, chars.len);
        std.mem.copy(u8, heapChars, chars);

        return self.allocateString(heapChars);
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
