const std = @import("std");

const Shape = union(enum) {
    circle: f32,
    rectangle: struct {
        width: f32,
        height: f32,
    },
    hyperrectangle: std.ArrayList(f32),
};

fn shapeArea(shape: Shape) f32 {
    return switch (shape) {
        .circle => |radius| std.math.pi * radius * radius,
        .rectangle => |rect| rect.width * rect.height,
        .hyperrectangle => |sides| {
            var p: f32 = 1.0;
            for (sides.items) |s| {
                p *= s;
            }
            return p;
        }
    };
}

fn optionalShapeArea(maybeShape: ?Shape) f32 {
    if (maybeShape) |shape| {
        return shapeArea(shape);
    } else {
        return 0.0;
    }
}

fn lessThan(context: void, a: u32, b: u32) std.math.Order {
    _ = context;
    return std.math.order(a, b);
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Optional and discriminated union.
    var shapes: [4]?Shape = undefined;
    var list = std.ArrayList(f32).init(allocator);
    try list.appendSlice(&[_]f32{ 1.1, 2.5, 3.14, 4.2, 5.0 });
    shapes[0] = Shape{ .circle = 5.0 };
    shapes[1] = Shape{ .rectangle = .{ .width = 4.0, .height = 6.0 } };
    shapes[2] = Shape{ .hyperrectangle = list };
    shapes[3] = null;
    
    var total_area: f32 = 0.0;
    for (shapes) |maybeShape| {
        total_area += optionalShapeArea(maybeShape);
    }

    // Initialize empty containers
    var array = std.ArrayList(u32).init(allocator);
    var map = std.AutoHashMap(u64, u32).init(allocator);
    var string_builder = std.ArrayList(u8).init(allocator);
    var string_map = std.StringHashMap(f64).init(allocator);
    var bounded_array = std.BoundedArray(i16, 210).init(0) catch unreachable;
    var array_map = std.ArrayHashMap(u8, bool, std.array_hash_map.AutoContext(u8), false).init(allocator);
    
    const Person = struct {
        name: []const u8,
        age: u8,
    };
    var people = std.MultiArrayList(Person){};
    
    const PriorityQueue = std.PriorityQueue(u32, void, lessThan);
    var queue = PriorityQueue.init(allocator, {});
    
    var sll = std.SinglyLinkedList(u64){};
    var buf_set = std.BufSet.init(allocator);
    var bit_set = std.bit_set.IntegerBitSet(512).initEmpty();
    var seg_list = std.SegmentedList(usize, 16){};

    // Add 200 elements to each container in a loop
    for (0..200) |i| {
        // ArrayList
        try array.append(@intCast(i));
        
        // HashMap
        try map.put(@intCast(i*10), @intCast(i*100));
        
        // String builder (will convert to string later)
        try string_builder.append(@intCast(65 + (i % 26))); // ASCII values for A-Z
        
        // StringHashMap
        const str_key = try std.fmt.allocPrint(allocator, "val{d}", .{i});
        try string_map.put(str_key, @as(f64, @floatFromInt(i)) / 10.0);
        
        // BoundedArray (only add up to capacity)
        if (i < bounded_array.capacity()) {
            try bounded_array.append(@as(i16, @intCast(i)) - 100);
        }
        
        // ArrayHashMap (only use u8 keys)
        if (i < 256) {
            try array_map.put(@intCast(i), i % 2 == 0);
        }
        
        // MultiArrayList
        const name = try std.fmt.allocPrint(allocator, "Person{d}", .{i});
        try people.append(allocator, .{ .name = name, .age = @intCast(i % 100) });
        
        // PriorityQueue
        try queue.add(@intCast(i * 5));
        
        // SinglyLinkedList
        const node = try allocator.create(@TypeOf(sll).Node);
        node.* = .{ .data = i * 1000, .next = sll.first };
        sll.first = node;
        
        // BufSet
        const set_item = try std.fmt.allocPrint(allocator, "item{d}", .{i});
        try buf_set.insert(set_item);
        
        // BitSet
        if (i < bit_set.capacity()) {
            bit_set.set(i);
        }
        
        // SegmentedList
        try seg_list.append(allocator, i * 10);
    }
    
    // Final string from builder
    const string = string_builder.items;

    // Use std.mem.doNotOptimizeAway with proper casts
    std.mem.doNotOptimizeAway(
        array.items[0] +
        map.get(40).? +
        @as(u32, string[0]) +
        @as(u32, @intFromFloat(string_map.get("val1").?)) +
        @as(u32, @as(u16, @bitCast(bounded_array.buffer[0]))) +
        @as(u32, @intFromBool(array_map.get(1).?)) +
        @as(u32, people.items(.age)[0]) +
        queue.items[0] +
        @as(u32, @truncate(sll.first.?.data)) +
        @as(u32, @intFromBool(buf_set.contains("item1"))) +
        @as(u32, @intFromBool(bit_set.isSet(1))) +
        @as(u32, @intCast(seg_list.at(0).*)) +
        @as(u32, @intFromFloat(total_area))
    );

    // Debug trap
    @breakpoint();
}
