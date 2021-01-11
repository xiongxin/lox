const std = @import("std");
const print = std.debug.print;

pub const Value = union(enum) {
    boolean: bool,
    number: f64,
    nil,
};

pub fn printValue(value: Value) void {
    switch (value) {
        .boolean => |boolean| print("{}", .{boolean}),
        .number => |number| print("{d:.5}", .{number}),
        .nil => print("nil", .{}),
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

pub fn AS_BOOL(value: Value) bool {
    return switch (value) {
        .boolean => |boolean| boolean,
        else => undefined,
    };
}

pub fn AS_NUMBER(value: Value) f64 {
    return switch (value) {
        .number => |number| number,
        else => undefined,
    };
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
