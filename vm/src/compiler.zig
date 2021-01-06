const std = @import("std");
const print = std.debug.print;

usingnamespace @import("common.zig");
usingnamespace @import("value.zig");
usingnamespace @import("chunk.zig");
usingnamespace @import("scanner.zig");

pub const Compiler = struct {
    pub fn init() Compiler {
        return Compiler{};
    }

    pub fn compile(self: *Compiler, source: []const u8) void {
        var scanner = Scanner.init(source);

        var line: usize = 0;
        while (true) {
            const token = scanner.scanToken();
            if (token.line != line) {
                print("{:>4} ", .{token.line});
                line = token.line;
            } else {
                print("   | ", .{});
            }

            print("{:>2} '{}'\n", .{ token.tokeType, token.literal });

            if (token.tokeType == .TOKEN_EOF) break;
        }
    }
};
