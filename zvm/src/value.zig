const std = @import("std");
const print = std.debug.print;
usingnamespace @import("chunk.zig");
usingnamespace @import("heap.zig");
pub const ArrayListOfValue = std.ArrayList(Value);

pub const Value = union(enum) {
    boolean: bool,
    number:  f64,
    nil,
    obj:     *Obj,
};

pub const ObjType = enum {
    str,
    fun,
    closure,
    upvalue,
};

pub const Obj = struct {
    objType:  ObjType,
    isMarked: bool,
    next:     ?*Obj,
};

pub const ArrayListOfObjUpvalue = std.ArrayList(*ObjUpvalue);

pub const ObjClosure = struct {
    obj: Obj,
    fun: *ObjFunction,
    upvalues: ArrayListOfObjUpvalue,
};

pub const ObjFunction = struct {
    obj: Obj,
    arity: usize,
    chunk: *Chunk,
    name: ?*ObjString,
    upvalueCount: u8,
};

pub const ObjUpvalue = struct {
    obj: Obj,
    closed: Value,
    location: *Value,
    next: ?*ObjUpvalue,
};

pub const ObjString = struct {
    obj: Obj,
    chars: []u8,
};

pub fn number2Value(value: f64) Value {
    return Value { .number = value};
}

pub fn nil2Value() Value {
    return Value.nil;
}

pub fn bool2Value(boolean: bool) Value {
    return Value { .boolean = boolean };
}

pub fn obj2Value(obj: *Obj) Value {
    return Value { .obj = obj };
}

pub fn objString2Value(str: *ObjString) Value {
    return Value { .obj = @ptrCast(*Obj, str) };
}

pub fn objFunction2Value(fun: *ObjFunction) Value {
    return Value { .obj = @ptrCast(*Obj, fun) };
}

pub fn objClosure2Value(closure: *ObjClosure) Value {
    return Value { .obj = @ptrCast(*Obj, closure) };
}

pub fn objUpvalue2Value(upvalue: *ObjUpvalue) Value {
    return Value { .obj = @ptrCast(*Obj, upvalue) };
}

pub fn asBool(value: Value) bool {
    return switch (value) {
        .boolean => |boolean| boolean,
        else => unreachable,
    };
}

pub fn asNumber(value: Value) f64 {
    return switch (value) {
        .number => |number| number,
        else => unreachable,
    };
}

pub fn asObj(value: Value) *Obj {
    return switch (value) {
        .obj => |obj| obj,
        else => unreachable,
    };
}

pub fn asString(value: Value) *ObjString {
    return @ptrCast(*ObjString, asObj(value));
}

pub fn asFunction(value: Value) *ObjFunction {
    return @ptrCast(*ObjFunction, asObj(value));
}

pub fn asClosure(value: Value) *ObjClosure {
    return @ptrCast(*ObjClosure, asObj(value));
}

pub fn asUpvalue(value: Value) *ObjUpvalue {
    return @ptrCast(*ObjUpvalue, asObj(value));
}

pub fn isBool(value: Value) bool {
    return switch (value) {
        .boolean => true,
        else => false,
    };
}

pub fn isNil(value: Value) bool {
    return switch (value) {
        .nil => true,
        else => false,
    };
}

pub fn isNumber(value: Value) bool {
    return switch (value) {
        .number => true,
        else => false,
    };
}

pub fn isUpvalue(value: Value) bool {
    return switch (value) {
        .obj => |obj| {
            return switch (obj.objType) {
                .upvalue => true,
                else => false,
            };
        },
        else => false,
    };
}

pub fn isString(value: Value) bool {
    return switch (value) {
        .obj => |obj| {
            return switch (obj.objType) {
                .str => true,
                else => false,
            };
        },
        else => false,
    };
}

pub fn isFun(value: Value) bool {
    return switch (value) {
        .obj => |obj| {
            return switch (obj.objType) {
                .str => true,
                else => false,
            };
        },
        else => false,
    };
}


pub fn isClosure(value: Value) bool {
    return switch (value) {
        .obj => |obj| {
            return switch (obj.objType) {
                .closure => true,
                else => false,
            };
        },
        else => false,
    };
}

pub fn isObj(value: Value) bool {
    return switch (value) {
        .obj => true,
        else => false,
    };
}

// 仅 nil 和 false 是 falsey
pub fn isFalsey(value: Value) bool {
    return isNil(value) or
        (isBool(value) and !asBool(value));
}

pub fn valuesEqual(a: Value, b: Value) bool {
    return switch (a) {
        .nil     => if (isNil(b)) true else false,
        .boolean => if (isBool(b)) asBool(a) == asBool(b) else false,
        .number  => if (isNumber(b)) asNumber(a) == asNumber(b) else false,
        .obj     => |obj| {
            print("{} {}", .{@ptrToInt(asObj(a)), @ptrToInt(asObj(b))});
            return @ptrToInt(asObj(a)) == @ptrToInt(asObj(b));
        },
    };
}