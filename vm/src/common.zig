const std = @import("std");

usingnamespace @import("value.zig");

pub const ArrayListOfU8 = std.ArrayList(u8);
pub const ArrayListOfValue = std.ArrayList(Value);