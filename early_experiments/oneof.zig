const std = @import("std");
const Allocator = std.mem.Allocator;
const Parser = @import("parser.zig").Parser;
const Error = @import("parser.zig").Error;
const Literal = @import("literal.zig").Literal;

pub fn OneOf(comptime Value: type, comptime Reader: type) type {
    return struct {
        parser: Parser(Value, Reader) = .{
            ._parse = parse,
        },
        parsers: []*Parser(Value, Reader),
        const Self = @This();
        pub fn init(comptime parsers: []*Parser(Value, Reader)) Self {
            return Self{
                .parsers = parsers,
            };
        }
        inline fn parse(comptime parser: *Parser(Value, Reader), allocator: *Allocator, src: *Reader) Error!?Value {
            const self = @fieldParentPtr(Self, "parser", parser);
            for (self.parsers) |one_parser| {
                const result = try one_parser(allocator, src);
                if (result != null) {
                    return result;
                }
            }
            return null;
        }
    };
}

// i'm being stupid today. idk how to fix it. go tomorrow i guess
test "oneof" {
    var allocator = std.testing.allocator;
    var reader = std.io.fixedBufferStream("abcdef");
    comptime var Mytype = Parser([]const u8, []u8);
    comptime var dd: []*Mytype = &.{
        &Literal(@TypeOf(reader)).init("dog").parser,
        &Literal(@TypeOf(reader)).init("cat").parser,
        &Literal(@TypeOf(reader)).init("pong").parser,
    };

    comptime var one_of = OneOf([]u8, @TypeOf(reader)).init(dd);
    //comptime var one_of = OneOf([]u8, @TypeOf(reader)).init(&.{
    //    &Literal(@TypeOf(reader)).init("dog").parser,
    //    &Literal(@TypeOf(reader)).init("cat").parser,
    //    &Literal(@TypeOf(reader)).init("pong").parser,
    //});

    comptime var want: []const u8 = "abc";
    _ = want;
    comptime var p = &one_of.parser;
    var result = try p.parse(&allocator, &reader);
    if (result == null) {
        std.debug.print("result is null\n");
    } else {
        std.debug.print("result = {?s}\n", .{result});
    }
    //try std.testing.expectEqualStrings(want, result.?);
    if (result) |r| {
        allocator.free(r);
    }
}
