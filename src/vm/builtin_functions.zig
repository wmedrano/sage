const Val = @import("val.zig").Val;
const Vm = @import("vm.zig").Vm;

const Error = Val.Function.Error;

pub const builtin_functions = [_]Val.Function{
    .{ .name = "%define", .is_static = true, .function = .{ .native = defineFunction } },
    .{ .name = "+", .is_static = true, .function = .{ .native = addFunction } },
    .{ .name = "-", .is_static = true, .function = .{ .native = subFunction } },
    .{ .name = "*", .is_static = true, .function = .{ .native = multFunction } },
    .{ .name = "/", .is_static = true, .function = .{ .native = divFunction } },
    .{ .name = "string-length", .is_static = true, .function = .{ .native = stringLengthFunction } },
    .{ .name = "<", .is_static = true, .function = .{ .native = lessFunction } },
};

fn stringLengthFunction(_: *Vm, args: []Val) Error!Val {
    if (args.len != 1) {
        return error.RuntimeError;
    }
    switch (args[0]) {
        Val.Type.string => |s| return .{ .int = @intCast(s.asSlice().len) },
        else => return error.RuntimeError,
    }
}

fn negate(arg: Val) Error!Val {
    switch (arg) {
        Val.Type.float => |f| return .{ .float = -f },
        Val.Type.int => |i| return .{ .int = -i },
        else => return error.RuntimeError,
    }
}

fn defineFunction(vm: *Vm, args: []Val) Val.Function.Error!Val {
    switch (args.len) {
        2 => {
            switch (args[0]) {
                .symbol => |s| {
                    try vm.defineVal(s.asSlice(), args[1]);
                },
                else => return error.RuntimeError,
            }
        },
        else => return error.RuntimeError,
    }
    return .void;
}

fn addFunction(_: *Vm, args: []Val) Error!Val {
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

fn subFunction(vm: *Vm, args: []Val) Error!Val {
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

fn reciprocal(arg: Val) Error!Val {
    switch (arg) {
        Val.Type.float => |f| return .{ .float = 1 / f },
        Val.Type.int => |i| {
            const f = @as(f64, @floatFromInt(i));
            return .{ .float = 1 / f };
        },
        else => return error.RuntimeError,
    }
}

fn multFunction(_: *Vm, args: []Val) Error!Val {
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

fn divFunction(vm: *Vm, args: []Val) Error!Val {
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

fn lessFunction(_: *Vm, args: []Val) Error!Val {
    if (args.len < 2) {
        return Error.RuntimeError;
    }
    var smaller = switch (args[0]) {
        .int => |i| i,
        else => return Error.RuntimeError,
    };
    for (args[1..]) |larger| {
        switch (larger) {
            .int => |i| {
                if (smaller >= i) return .{ .boolean = false };
                smaller = i;
            },
            else => return Error.RuntimeError,
        }
    }
    return .{ .boolean = true };
}
