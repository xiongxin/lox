const std = @import("std");
const print = std.debug.print;

pub const Value = union(enum) {
    boolean: bool,
    number: f64,
    nil,
    obj: *Obj,
};

pub const ObjType = enum {
    OBJ_STRING,
};

pub const Obj = struct {
    objType: ObjType,
    next: ?*Obj,
};

pub const ObjString = struct {
    obj: Obj,
    chars: []u8,
};

pub fn printValue(value: Value) void {
    switch (value) {
        .boolean => |boolean| print("{}", .{boolean}),
        .number => |number| print("{d:.5}", .{number}),
        .nil => print("nil", .{}),
        .obj => |obj| printObject(obj),
    }
}

pub fn printObject(obj: *Obj) void {
    switch (obj.objType) {
        .OBJ_STRING => print("{}", .{OBJ_AS_STRING(obj).chars}),
    }
}

pub fn BOOL_VAL(value: bool) Value {
    return Value{ .boolean = value };
}

pub fn NIL_VAL() Value {
    return Value.nil;
}

pub fn NUMBER_VAL(value: f64) Value {
    return Value{ .number = value };
}

pub fn OBJ_VAL(obj: *Obj) Value {
    return Value{ .obj = obj };
}

pub fn OBJ_STRING_VAL(objString: *ObjString) Value {
    return OBJ_VAL(@ptrCast(*Obj, objString));
}

pub fn AS_BOOL(value: Value) bool {
    return switch (value) {
        .boolean => |boolean| boolean,
        else => unreachable,
    };
}

pub fn AS_NUMBER(value: Value) f64 {
    return switch (value) {
        .number => |number| number,
        else => unreachable,
    };
}

pub fn AS_OBJ(value: Value) *Obj {
    return switch (value) {
        .obj => |obj| obj,
        else => unreachable,
    };
}

pub fn AS_STRING(value: Value) *ObjString {
    return @ptrCast(*ObjString, AS_OBJ(value));
}

pub fn OBJ_AS_STRING(obj: *Obj) *ObjString {
    return @ptrCast(*ObjString, obj);
}

pub fn IS_BOOL(value: Value) bool {
    return switch (value) {
        .boolean => true,
        else => false,
    };
}

pub fn IS_NIL(value: Value) bool {
    return switch (value) {
        .nil => true,
        else => false,
    };
}

pub fn IS_NUMBER(value: Value) bool {
    return switch (value) {
        .number => true,
        else => false,
    };
}

pub fn IS_OBJ(value: Value) bool {
    return switch (value) {
        .obj => true,
        else => false,
    };
}

pub fn OBJ_TYPE(value: Value) ObjType {
    return AS_OBJ(value).objType;
}

pub fn IS_STRING(value: Value) bool {
    return isObjType(value, .OBJ_STRING);
}

fn isObjType(value: Value, objType: ObjType) bool {
    return IS_OBJ(value) and AS_OBJ(value).objType == objType;
}

pub fn valuesEqual(a: Value, b: Value) bool {
    switch (a) {
        .boolean => |boolean| {
            if (IS_BOOL(b)) {
                return AS_BOOL(b) == boolean;
            } else {
                return false;
            }
        },
        .number => |number| {
            if (IS_NUMBER(b)) {
                return AS_NUMBER(b) == number;
            } else {
                return false;
            }
        },
        .nil => {
            return if (IS_NIL(b)) true else false;
        },
        .obj => |obj| {
            return if (IS_OBJ(b)) obj == AS_OBJ(b) else false;
        },
    }
}
