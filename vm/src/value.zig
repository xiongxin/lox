const std = @import("std");
const print = std.debug.print;

pub const Value = f64;


pub fn printValue(self: Value) void {
    print("{d:.5}", .{self});
}