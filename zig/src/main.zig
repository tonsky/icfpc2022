const std = @import("std");
const zigimg = @import("zigimg");

const Allocator = std.mem.Allocator;

const Coord = i64;

const Point = struct { x: Coord, y: Coord };

const Rect = struct {
    l: Coord,
    b: Coord,
    r: Coord,
    t: Coord,
    const FULL = Rect{ .l = 0, .b = 0, .r = 400, .t = 400 };

    pub fn area(self: Rect) i64 {
        return (self.t - self.b) * (self.r - self.l);
    }

    pub fn contains(self: Rect, point: Point) bool {
        return self.l <= point.x and point.x < self.r and self.b <= point.y and point.y < self.t;
    }
};

const Color = zigimg.color.Rgba32;

pub fn eqlColor(c1: Color, c2: Color) bool {
    return c1.r == c2.r and c1.g == c2.g and c1.b == c2.b;
}

pub fn writeColor(c: Color, out: anytype) !void {
    try out.print("#{x}{x}{x}", .{ c.r, c.g, c.b });
}

const WHITE = Color{ .r = 255, .g = 255, .b = 255, .a = 255 };

const SimpleBlock = struct {
    rect: Rect,
    color: Color,

    pub fn write(self: SimpleBlock, out: anytype) !void {
        try out.print("({}, {})x({}, {}) ", .{ self.rect.l, self.rect.b, self.rect.r, self.rect.t });
        try writeColor(self.color, out);
        try out.writeAll("\n");
    }
};

const ComplexBlock = struct {
    rect: Rect,
    children: []SimpleBlock,

    pub fn write(self: ComplexBlock, out: anytype) !void {
        try out.print("({}, {})x({}, {}):\n", .{ self.rect.l, self.rect.b, self.rect.r, self.rect.t });
        for (self.children) |block| {
            try out.writeAll("  ");
            try block.write(out);
        }
    }
};

const BlockTypeTag = enum { simple, complex };

const Block = union(BlockTypeTag) {
    simple: SimpleBlock,
    complex: ComplexBlock,

    pub fn write(self: Block, out: anytype) !void {
        switch (self) {
            .simple => try self.simple.write(out),
            .complex => try self.complex.write(out),
        }
    }

    pub fn contains(self: Block, p: Point) bool {
        switch (self) {
            .simple => return self.simple.rect.contains(p),
            .complex => return self.complex.rect.contains(p),
        }
    }
};

pub fn str(alloc: Allocator, strings: []const []const u8) ![]const u8 {
    var len: usize = 0;
    for (strings) |s| {
        len += s.len;
    }
    var bytes = try alloc.alloc(u8, len);
    var i: usize = 0;
    for (strings) |s| {
        std.mem.copy(u8, bytes[i..], s);
        i += s.len;
    }
    return bytes;
}

const Id = []const u8;

const Picture = struct {
    blocks: std.StringHashMap(Block),
    allocator: Allocator,

    pub fn init(allocator: Allocator) !Picture {
        var blocks = std.StringHashMap(Block).init(allocator);
        var id = try str(allocator, &[_][]const u8{"0"});
        try blocks.putNoClobber(id, Block{ .simple = SimpleBlock{ .rect = Rect.FULL, .color = WHITE } });
        return Picture{ .blocks = blocks, .allocator = allocator };
    }

    pub fn deinit(self: *Picture) void {
        var it = self.blocks.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            // self.allocator.free(entry.value_ptr.*);
        }
        self.blocks.deinit();
    }

    pub fn transform(self: *Picture, op: Op) !void {
        switch (op) {
            .color => {
                var entry = self.blocks.getEntry(op.color.id) orelse return error.IdNotFound;
                var block_ptr = entry.value_ptr;
                switch (block_ptr.*) {
                    .simple => block_ptr.simple.color = op.color.color,
                    .complex => block_ptr.* = Block{ .simple = SimpleBlock{ .rect = block_ptr.complex.rect, .color = op.color.color } },
                }
            },
            .xcut => {
                const kv = self.blocks.fetchRemove(op.xcut.id) orelse return error.IdNotFound;
                const id = kv.key;
                defer self.allocator.free(id);
                const block = kv.value;
                switch (block) {
                    .simple => {
                        const rect = block.simple.rect;
                        const color = block.simple.color;
                        const left = Block{ .simple = SimpleBlock{ .rect = Rect{ .l = rect.l, .b = rect.b, .r = op.xcut.x, .t = rect.t }, .color = color } };
                        const right = Block{ .simple = SimpleBlock{ .rect = Rect{ .l = op.xcut.x, .b = rect.b, .r = rect.r, .t = rect.t }, .color = color } };
                        const left_id = try str(self.allocator, &.{ id, ".0" });
                        try self.blocks.putNoClobber(left_id, left);
                        const right_id = try str(self.allocator, &.{ id, ".1" });
                        try self.blocks.putNoClobber(right_id, right);
                    },
                    else => return error.ComplexNotSupported,
                }
            },
            else => return error.UnknownOp,
        }
    }

    pub fn write(self: Picture, out: anytype) !void {
        var it = self.blocks.iterator();
        while (it.next()) |entry| {
            try out.print("{s}: ", .{entry.key_ptr.*});
            try entry.value_ptr.*.write(out);
        }
    }

    pub fn block_at(self: Picture, point: Point) !Block {
        var it = self.blocks.iterator();
        while (it.next()) |entry| {
            if (entry.value_ptr.contains(point)) {
                return entry.value_ptr.*;
            }
        }
        return error.BlockNotFound;
    }
};

const OpTypeTag = enum { color, xcut, ycut, pcut, swap, merge };

const ColorOp = struct { id: Id, color: Color };
const XCutOp = struct { id: Id, x: Coord };
const YCutOp = struct { id: Id, y: Coord };
const PCutOp = struct { id: Id, x: Coord, y: Coord };
const SwapOp = struct { id1: Id, id2: Id };
const MergeOp = struct { id1: Id, id2: Id };

const Op = union(OpTypeTag) { color: ColorOp, xcut: XCutOp, ycut: YCutOp, pcut: PCutOp, swap: SwapOp, merge: MergeOp };

const Log = []Op;

pub fn dist(c1: Color, c2: Color) f64 {
    var dr = @intToFloat(f64, c1.r) - @intToFloat(f64, c2.r);
    var dg = @intToFloat(f64, c1.g) - @intToFloat(f64, c2.g);
    var db = @intToFloat(f64, c1.b) - @intToFloat(f64, c2.b);
    return @sqrt(dr * dr + dg * dg + db * db);
}

const PixelIter = struct {
    pixels: []Color,
    rect: Rect,
    y: Coord,
    x: Coord,

    pub fn init(pixels: []Color, rect: Rect) PixelIter {
        return PixelIter{ .pixels = pixels, .rect = rect, .y = rect.t - 1, .x = rect.l };
    }

    pub fn next(self: *PixelIter) ?Color {
        while (self.x >= self.rect.r and self.y >= self.rect.b) {
            self.x = self.rect.l;
            self.y -= 1;
        }

        if (self.y < self.rect.b) {
            return null;
        }

        var idx = @intCast(usize, (399 - self.y) * 400 + self.x);
        self.x += 1;

        return self.pixels[idx];
    }
};

pub fn similarity(pixels: []Color, color: Color, rect: Rect) i64 {
    var res: f64 = 0;
    var iter = PixelIter.init(pixels, rect);
    while (iter.next()) |c| {
        res += dist(c, color);
    }
    return @floatToInt(i64, @round(res * 0.005));
}

pub fn average(pixels: []Color, rect: Rect) Color {
    var r: i64 = 0;
    var g: i64 = 0;
    var b: i64 = 0;
    var iter = PixelIter.init(pixels, rect);
    while (iter.next()) |c| {
        r += c.r;
        g += c.g;
        b += c.b;
    }
    var area = rect.area();
    return Color{ .r = @intCast(u8, @divTrunc(r, area)), .g = @intCast(u8, @divTrunc(g, area)), .b = @intCast(u8, @divTrunc(b, area)), .a = 255 };
}

var a: std.mem.Allocator = undefined;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();
    a = arena.allocator();

    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    var image = try zigimg.Image.fromFilePath(a, "../resources/1.png");
    defer image.deinit();
    try stdout.print("Dimensions: {}x{}\n", .{ image.width, image.height });
    switch (image.pixels) {
        .rgba32 => {
            var colors = image.pixels.rgba32;
            try stdout.print("rgba32, first [{}, {}, {}, {}]\n", .{ colors[0].r, colors[0].g, colors[0].b, colors[0].a });
            try stdout.print("last [{}, {}, {}, {}]\n", .{ colors[colors.len - 1].r, colors[colors.len - 1].g, colors[colors.len - 1].b, colors[colors.len - 1].a });
            try stdout.print("similarity {}\n", .{similarity(colors, WHITE, Rect.FULL)});
            try stdout.print("average {}\n", .{average(colors, Rect.FULL)});
        },
        else => try stdout.print("???\n", .{}),
    }

    var picture = try Picture.init(a);
    defer picture.deinit();

    try bw.flush(); // don't forget to flush!
}

const expect = std.testing.expect;
const expectError = std.testing.expectError;

test "transform" {
    var allocator = std.testing.allocator;
    var picture = try Picture.init(allocator);
    defer picture.deinit();
    var red = Color{ .r = 255, .g = 0, .b = 0, .a = 255 };
    var out = std.io.getStdErr().writer();
    // try picture.write(out);
    try picture.transform(Op{ .color = ColorOp{ .id = "0", .color = red } });

    var block = try picture.block_at(Point{ .x = 200, .y = 200 });
    try expect(eqlColor(block.simple.color, red));

    try expectError(error.IdNotFound, picture.transform(Op{ .color = ColorOp{ .id = "1", .color = red } }));
    try expectError(error.BlockNotFound, picture.block_at(Point{ .x = 500, .y = 200 }));

    try picture.transform(Op{ .xcut = XCutOp{ .id = "0", .x = 100 } });
    try picture.write(out);
}

test "load image" {
    var allocator = std.testing.allocator;
    var image = try zigimg.Image.fromFilePath(allocator, "../resources/1.png");
    defer image.deinit();

    try expect(image.width == 400);
    try expect(image.height == 400);
    try expect(image.pixels == .rgba32);
    var colors = image.pixels.rgba32;
    try expect(eqlColor(colors[0], Color{ .r = 69, .g = 123, .b = 195, .a = 255 }));
    try expect(eqlColor(colors[colors.len - 1], Color{ .r = 0, .g = 74, .b = 173, .a = 255 }));
    try expect(similarity(colors, WHITE, Rect.FULL) == 194616);
    try expect(eqlColor(average(colors, Rect.FULL), Color{ .r = 99, .g = 115, .b = 137, .a = 255 }));
}
