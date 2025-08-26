const std = @import("std");
const global_allocator = std.heap.c_allocator;

fn printFmt(comptime fmt: []const u8, args: anytype) !void {
    var buf: [256]u8 = undefined;
    const out = try std.fmt.bufPrint(&buf, fmt, args);
    _ = try std.posix.write(std.posix.STDOUT_FILENO, out);
}

pub fn main() !void {
    const args = try std.process.argsAlloc(global_allocator);
    defer std.process.argsFree(global_allocator, args);
    if (args.len > 1) {
        try printFmt("Hello world {s}!\n", .{args[1]});
    } else {
        try printFmt("Hello world!\n", .{});
    }
}
