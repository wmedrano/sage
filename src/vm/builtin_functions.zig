const Val = @import("val.zig").Val;
const Vm = @import("vm.zig").Vm;

pub const builtin_functions = [_]Val.Function{
    .{ .name = "+", .is_static = true, .function = .{ .native = addFunction } },
    .{ .name = "-", .is_static = true, .function = .{ .native = subFunction } },
    .{ .name = "*", .is_static = true, .function = .{ .native = multFunction } },
    .{ .name = "/", .is_static = true, .function = .{ .native = divFunction } },
    .{ .name = "string-length", .is_static = true, .function = .{ .native = stringLengthFunction } },
};

fn stringLengthFunction(_: *Vm, args: []Val) !Val {
    if (args.len != 1) {
        return error.RuntimeError;
    }
    switch (args[0]) {
        Val.Type.string => |s| return .{ .int = @intCast(s.data.len) },
        else => return error.RuntimeError,
    }
}

fn negate(arg: Val) !Val {
    switch (arg) {
        Val.Type.float => |f| return .{ .float = -f },
        Val.Type.int => |i| return .{ .int = -i },
        else => return error.RuntimeError,
    }
}

fn addFunction(_: *Vm, args: []Val) !Val {
    var int_sum: i64 = 0;
    var float_sum: f64 = 0.0;
    var has_float = false;
    for (args) |arg| {
        switch (arg) {
            Val.Type.float => |f| {
                has_float = true;
                float_sum += f;
            },
            Val.Type.int => |i| int_sum += i,
            else => return error.RuntimeError,
        }
    }
    if (has_float) {
        float_sum += @floatFromInt(int_sum);
        return .{ .float = float_sum };
    }
    return .{ .int = int_sum };
}

fn subFunction(vm: *Vm, args: []Val) !Val {
    switch (args.len) {
        0 => return error.RuntimeError,
        1 => return negate(args[0]),
        2 => {
            var factors = [2]Val{ args[0], try negate(args[1]) };
            return addFunction(vm, &factors);
        },
        else => {
            const sub_part = try addFunction(vm, args[1..]);
            var parts = [2]Val{ args[0], try negate(sub_part) };
            return addFunction(vm, &parts);
        },
    }
}

fn reciprocal(arg: Val) !Val {
    switch (arg) {
        Val.Type.float => |f| return .{ .float = 1 / f },
        Val.Type.int => |i| {
            const f = @as(f64, @floatFromInt(i));
            return .{ .float = 1 / f };
        },
        else => return error.RuntimeError,
    }
}

fn multFunction(_: *Vm, args: []Val) !Val {
    var int_prod: i64 = 1;
    var float_prod: f64 = 1.0;
    var has_float = false;
    for (args) |arg| {
        switch (arg) {
            Val.Type.float => |f| {
                has_float = true;
                float_prod *= f;
            },
            Val.Type.int => |i| int_prod *= i,
            else => return error.RuntimeError,
        }
    }
    if (has_float) {
        float_prod *= @floatFromInt(int_prod);
        return .{ .float = float_prod };
    }
    return .{ .int = int_prod };
}

fn divFunction(vm: *Vm, args: []Val) !Val {
    switch (args.len) {
        0 => return error.RuntimeError,
        1 => return reciprocal(args[0]),
        2 => {
            var parts = [2]Val{ args[0], try reciprocal(args[1]) };
            return multFunction(vm, &parts);
        },
        else => {
            const divisor = try multFunction(vm, args[1..]);
            var factors = [2]Val{ args[0], try reciprocal(divisor) };
            return multFunction(vm, &factors);
        },
    }
}
