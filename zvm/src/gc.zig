const std = @import("std");
const print = std.debug.print;

usingnamespace @import("value.zig");
usingnamespace @import("chunk.zig");
usingnamespace @import("compiler.zig");
usingnamespace @import("heap.zig");
usingnamespace @import("vm.zig");

pub const GCLOG = std.log.scoped(.GC);

pub fn collectGarbage() !void {
    GCLOG.info("-- gc begin", .{});

    try markRoots();
    try traceReferences();
    //try sweep();

    GCLOG.info("-- gc end", .{});
}

fn traceReferences() !void {
    while (vm.grayStack.items.len > 0) {
       try blackenObject(vm.grayStack.pop());
    }
}

// A black object is any object whose isMarked field is set and that is no longer in the gray stack.
fn blackenObject(obj: *Obj) !void {
    switch (obj.objType) {
        .str => GCLOG.info("{} blacken {}", .{ @ptrToInt(obj), @ptrCast(*ObjString, obj).chars }),
        .fun => {
            const fun = @ptrCast(*ObjFunction, obj);
            if (fun.name) |name| {
                GCLOG.info("{} blacken <fn {}>", .{ @ptrToInt(obj), name.chars });
            } else {
                GCLOG.info("{} blacken <script>", .{@ptrToInt(obj)});
            }
        },
        .closure => {
            try printValue(objFunction2Value(@ptrCast(*ObjClosure, obj).fun));
        },
        .upvalue => GCLOG.info("{} blacken upvalue", .{@ptrToInt(obj)}),
    }


    switch (obj.objType) {
        .str => return,
        .upvalue => try markValue(&@ptrCast(*ObjUpvalue, obj).closed),
        .fun => {
            const fun = @ptrCast(*ObjFunction, obj);
            if (fun.name) |name| {
                try markObject(@ptrCast(*Obj, name));
            }
            try markArray(&fun.chunk.constants);
        },
        .closure => {
            const closure = @ptrCast(*ObjClosure, obj);
            try markObject(@ptrCast(*Obj, closure.fun));
            for (closure.upvalues.items) |*item| {
                try markObject(@ptrCast(*Obj, item));
            }
        }
    }
}

fn markArray(array: *ArrayListOfValue) !void {
    for (array.items) |*item| {
        try markValue(item);
    }
}


// ROOT分析
fn markRoots() !void {
    // 1. 操作栈
    for (vm.stack[0..vm.stackTop]) |*value| {
        try markValue(value);
    }

    var i: usize = 0;
    while (i < vm.frameCount) : (i += 1) {
        try markObject(@ptrCast(*Obj, vm.frames[i].closure));
    }

    var upvalue = vm.openUpvalue;
    while (upvalue) |uv| {
        try markObject(@ptrCast(*Obj, uv));
        upvalue = uv.next;
    }

    // 2. 全局变量
    try markGlobals(&vm.globals);

    // 编译状态
    try markCompilerRoots();
}

fn markCompilerRoots() !void {
    var compiler = current;
    while (compiler) |c| {
        if (c.function) |fun| {
            try markObject(@ptrCast(*Obj, fun));
        }
        compiler = c.enclosing;
    }
}

fn markValue(value: *Value) !void {
    if (!isObj(value.*)) return;
    try markObject(asObj(value.*));
}

fn markObject(obj: *Obj) !void {
    if (obj.isMarked) return;
    
    switch (obj.objType) {
        .str => GCLOG.info("{} mark {}", .{ @ptrToInt(obj), @ptrCast(*ObjString, obj).chars }),
        .fun => {
            const fun = @ptrCast(*ObjFunction, obj);
            if (fun.name) |name| {
                GCLOG.info("{} mark <fn {}>", .{ @ptrToInt(obj), name.chars });
            } else {
                GCLOG.info("{} mark <script>", .{@ptrToInt(obj)});
            }
        },
        .closure => {
            try printValue(objFunction2Value(@ptrCast(*ObjClosure, obj).fun));
        },
        .upvalue => GCLOG.info("{} mark upvalue", .{@ptrToInt(obj)}),
    }

    obj.isMarked = true;

    try vm.grayStack.append(obj);
}

fn markGlobals(globals: *ObjStringHashOfValue) !void {
    var iterator = globals.iterator();
    while (iterator.next()) |kv| {
        try markObject(@ptrCast(*Obj, kv.key));
        try markValue(&kv.value);
    }
}
