const std = @import("std");
const process = std.process;
const print = std.debug.print;

usingnamespace @import("chunk.zig");
usingnamespace @import("vm.zig");

pub fn main() anyerror!void {
    var memory: [1024 * 1024]u8 = undefined;
    var arena = std.heap.ArenaAllocator.init(&std.heap.FixedBufferAllocator.init(memory[0..]).allocator);
    defer arena.deinit();

    var vm = VM.init(&arena.allocator);
    defer vm.deinit();
    vm.restStack();

    var args = process.args();
    _ = args.skip(); // skip prog name
    if (args.next(&arena.allocator)) |arg1| { // get filename
        if (arg1) |filename| {
            try runFile(&arena.allocator, filename, &vm);
            defer _ = arena.allocator.free(filename);
        } else |_| {}
    } else { // repl
        try repl(&arena.allocator, &vm);
    }
}

fn repl(allocator: *std.mem.Allocator, vm: *VM) !void {
    var buf: [1024]u8 = undefined;
    const stdIn = std.io.getStdIn().reader();
    print("Welcome to the Lox!\n", .{});
    print("> ", .{});
    while (stdIn.read(&buf)) |len| {
        var slice = buf[0 .. len + 1];
        slice[len] = 0;
        _ = vm.interpret(slice);
        print("> ", .{});
    } else |err| {
        process.exit(74);
    }
}

fn runFile(allocator: *std.mem.Allocator, path: []const u8, vm: *VM) !void {
    const file = try std.fs.openFileAbsolute(path, .{});
    const filesize = try file.getEndPos();
    const buf = try allocator.alloc(u8, filesize + 1);
    defer allocator.free(buf);
    const size = try file.readAll(buf);
    buf[filesize] = 0;
    _ = vm.interpret(buf);
}
