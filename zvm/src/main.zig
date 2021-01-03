const std = @import("std");

usingnamespace @import("chunk.zig");
usingnamespace @import("vm.zig");

const process = std.process;
const print = std.debug.print;

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    vm.init(allocator);
    defer vm.deinit();

    var args = process.args();
    _ = args.skip(); // skip prog name
    const filename = args.next(allocator);
    if (filename) |fm_o| { // get filename
        if (fm_o) |fm| {
            try runFile(allocator, fm);
            defer _ = allocator.free(fm);
        } else |_| {}
    } else { // repl
        try repl(allocator);
    }
}

fn repl(allocator: *std.mem.Allocator) !void {
    var buf: [1024]u8 = undefined;
    const stdIn = std.io.getStdIn().reader();
    print("Welcome to the Lox!\n", .{});
    print("> ", .{});
    while (stdIn.read(&buf)) |len| {
        var slice = buf[0 .. len + 1];
        slice[len] = 0;
        _ = try interpret(allocator, slice);
        print("> ", .{});
    } else |err| {
        process.exit(74);
    }
}

fn runFile(allocator: *std.mem.Allocator, filename: []const u8) !void {
    const file = try std.fs.openFileAbsolute(filename, .{});
    const filesize = try file.getEndPos();
    const buf = try allocator.alloc(u8, filesize + 1);
    defer allocator.free(buf);
    const size = try file.readAll(buf);
    buf[filesize] = 0;
    // 全局内存分配器
    _ = try interpret(allocator, buf);
}
