fn fib(n: usize) usize {
    if ( n < 2) return n;
    return fib(n - 2) + fib(n - 1);
}

const std = @import("std");


pub fn main() void {
    std.debug.print("{}\n", .{fib(40)});
}