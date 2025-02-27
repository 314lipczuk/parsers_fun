const std = @import("std");
const Allocator = std.mem.Allocator;
const Parser = @import("parser.zig").Parser;
const Error = @import("parser.zig").Error;

pub fn Literal(comptime Reader: type) type {
    return struct {
        parser: Parser([]u8, Reader) = .{
            ._parse = parse,
        },
        want: []const u8,

        const Self = @This();

        pub fn init(comptime want: []const u8) Self {
            return Self{ .want = want };
        }

        inline fn parse(comptime parser: *Parser([]u8, Reader), allocator: *Allocator, src: *Reader) Error!?[]u8 {
            const self = @fieldParentPtr(Self, "parser", parser);
            const buf = try allocator.alloc(u8, self.want.len);
            const read = try src.reader().readAll(buf);
            if (read < self.want.len or !std.mem.eql(u8, buf, self.want)) {
                try src.seekableStream().seekBy(-@as(i64, @intCast(read)));
                allocator.free(buf);
                return null;
            }
            return buf;
        }
    };
}

test "literal" {
    var allocator = std.testing.allocator;
    var reader = std.io.fixedBufferStream("abcdef");
    comptime var want: []const u8 = "abc";
    comptime var literal = Literal(@TypeOf(reader)).init(want);
    comptime var p = &literal.parser;
    var result = try p.parse(&allocator, &reader);
    try std.testing.expectEqualStrings(want, result.?);
    if (result) |r| {
        allocator.free(r);
    }
}

test "literal2" {
    var allocator = std.testing.allocator;
    var reader = std.io.fixedBufferStream("abcdef");
    comptime var want: []const u8 = "abc";
    comptime var literal = Literal(@TypeOf(reader)).init(want);
    comptime var p = &literal.parser;
    var result = try p.parse(&allocator, &reader);
    std.debug.print("result = {?s}\n", .{result});
    try std.testing.expectEqualStrings(want, result.?);
    if (result) |r| {
        allocator.free(r);
    }
}
