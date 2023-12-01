const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Error = error{
    EndOfStream,
    Utf8InvalidStartByte,
} || std.fs.File.ReadError || std.fs.File.SeekError || std.mem.Allocator.Error;

pub fn Parser(comptime Value: type, comptime Reader: type) type {
    return struct {
        const Self = @This();
        _parse: fn (comptime self: *Self, allocator: *Allocator, src: *Reader) callconv(.Inline) Error!?Value,

        pub inline fn parse(comptime self: *Self, allocator: *Allocator, src: *Reader) Error!?Value {
            return self._parse(self, allocator, src);
        }
    };
}
