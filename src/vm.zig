const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const ast = @import("ast.zig");
const val = @import("val.zig");
const bytecode = @import("bytecode.zig");

const FunctionFrame = struct {
    bytecode: *const bytecode.ByteCodeFunc,
    stack_start: usize,
    bytecode_idx: usize = 0,
};

pub const VmError = std.mem.Allocator.Error || error{
    RuntimeError,
    CorruptStack,
    WrongType,
    UndefinedSymbol,
    NotImplemented,
};

pub const Vm = struct {
    stack: std.ArrayList(val.Val),
    function_frames: std.ArrayList(FunctionFrame),
    builtin_functions: []const val.Function = &builtin_functions,

    pub fn init(alloc: std.mem.Allocator) VmError!Vm {
        const stack = try std.ArrayList(val.Val).initCapacity(alloc, 1024);
        const function_frames = try std.ArrayList(FunctionFrame).initCapacity(alloc, 1024);
        return .{
            .stack = stack,
            .function_frames = function_frames,
        };
    }

    pub fn deinit(self: *Vm) void {
        for (self.stack.items) |v| v.deinit(self.allocator());
        self.stack.deinit();
        self.function_frames.deinit();
    }

    pub fn allocator(self: *Vm) std.mem.Allocator {
        return self.stack.allocator;
    }

    fn run_bytecode(self: *Vm, bc: *const bytecode.ByteCodeFunc, args: []val.Val) VmError!val.Val {
        if (self.stack.items.len != 0) {
            return VmError.CorruptStack;
        }
        for (args) |arg| {
            try self.stack.append(try arg.clone(self.allocator()));
        }
        try self.function_frames.append(.{ .bytecode = bc, .stack_start = 0 });
        while (try self.run_next()) {}
        const ret = self.stack.popOrNull() orelse val.Val{ .int = 0 };
        self.clear_stack();
        return ret;
    }

    fn clear_stack(self: *Vm) void {
        for (self.stack.items) |v| v.deinit(self.allocator());
        self.stack.clearRetainingCapacity();
    }

    fn get_symbol(self: *Vm, symbol: []const u8) ?val.Val {
        for (self.builtin_functions) |*f| {
            if (std.mem.eql(u8, symbol, f.name)) {
                return val.Val{ .function = f };
            }
        }
        return null;
    }

    fn run_next(self: *Vm) VmError!bool {
        if (self.function_frames.items.len == 0) return false;
        const function_frame = &self.function_frames.items[self.function_frames.items.len - 1];
        const instruction = function_frame.bytecode.instructions.items[function_frame.bytecode_idx];
        switch (instruction) {
            bytecode.ByteCodeType.push_const => |i| try self.execute_push_const(function_frame.bytecode, i),
            bytecode.ByteCodeType.deref => try self.execute_deref(),
            bytecode.ByteCodeType.eval => |n| try self.execute_eval(n),
            bytecode.ByteCodeType.ret => try self.execute_ret(),
        }
        function_frame.bytecode_idx += 1;
        return true;
    }

    fn execute_push_const(self: *Vm, bc: *const bytecode.ByteCodeFunc, const_idx: usize) VmError!void {
        const v = try bc.constants.items[const_idx].clone(self.allocator());
        try self.stack.append(v);
    }

    fn execute_deref(self: *Vm) VmError!void {
        const v = try switch (self.stack.getLast()) {
            val.ValType.symbol => |s| self.get_symbol(s) orelse VmError.UndefinedSymbol,
            else => return VmError.WrongType,
        };
        const cloned_val = try v.clone(self.allocator());
        self.stack.pop().deinit(self.allocator());
        try self.stack.append(cloned_val);
    }

    fn execute_eval(self: *Vm, n: usize) VmError!void {
        const function_idx = self.stack.items.len - n;
        const stack = self.stack.items[function_idx + 1 ..];
        switch (self.stack.items[function_idx]) {
            val.ValType.function => |f| {
                const res = try f.function(stack);
                for (self.stack.items[function_idx..]) |v| v.deinit(self.allocator());
                try self.stack.resize(function_idx);
                try self.stack.append(res);
            },
            else => return VmError.WrongType,
        }
    }

    fn execute_ret(self: *Vm) VmError!void {
        const function_frame = self.function_frames.pop();
        if (self.stack.items.len <= function_frame.stack_start) {
            try self.stack.append(.{ .int = 0 });
            return;
        }
        const ret = self.stack.pop();
        for (self.stack.items[function_frame.stack_start..]) |v| v.deinit(self.allocator());
        try self.stack.resize(function_frame.stack_start);
        try self.stack.append(ret);
    }
};

const builtin_functions = [_]val.Function{
    .{ .name = "+", .function = add_function },
    .{ .name = "string-length", .function = string_length_function },
};

fn string_length_function(args: []val.Val) !val.Val {
    if (args.len != 1) {
        return error.RuntimeError;
    }
    switch (args[0]) {
        val.ValType.string => |s| return .{ .int = @intCast(s.len) },
        else => return error.RuntimeError,
    }
}

fn add_function(args: []val.Val) !val.Val {
    var int_sum: i64 = 0;
    var float_sum: f64 = 0.0;
    var has_float = false;
    for (args) |arg| {
        switch (arg) {
            val.ValType.float => |f| {
                has_float = true;
                float_sum += f;
            },
            val.ValType.int => |i| int_sum += i,
            else => return error.RuntimeError,
        }
    }
    if (has_float) {
        return error.NotImplemented;
    }
    return .{ .int = int_sum };
}

test "expression can eval" {
    const src = "(+ (string-length \"four\") -5)";
    const asts = try ast.AstCollection.init_with_str(src, std.testing.allocator);
    defer asts.deinit();

    var bc = try bytecode.ByteCodeFunc.init(&asts.asts[0], std.testing.allocator);
    defer bc.deinit();

    var vm = try Vm.init(std.testing.allocator);
    defer vm.deinit();
    const actual = try vm.run_bytecode(&bc, &[_]val.Val{});
    defer actual.deinit(vm.allocator());
    try std.testing.expectEqualDeep(
        val.Val{ .int = -1 },
        actual,
    );
}

test "successful expression clears stack" {
    const src = "(+ 1 2 3)";
    const asts = try ast.AstCollection.init_with_str(src, std.testing.allocator);
    defer asts.deinit();

    var bc = try bytecode.ByteCodeFunc.init(&asts.asts[0], std.testing.allocator);
    defer bc.deinit();

    var vm = try Vm.init(std.testing.allocator);
    defer vm.deinit();
    var v = try vm.run_bytecode(&bc, &[_]val.Val{});
    defer v.deinit(vm.allocator());

    try std.testing.expectEqualDeep(vm.stack.items, &[_]val.Val{});
}

test "wrong args halts VM and maintains VM state" {
    const src = "(+ 10 (string-length 4))";
    const asts = try ast.AstCollection.init_with_str(src, std.testing.allocator);
    defer asts.deinit();

    var bc = try bytecode.ByteCodeFunc.init(&asts.asts[0], std.testing.allocator);
    defer bc.deinit();

    var vm = try Vm.init(std.testing.allocator);
    defer vm.deinit();
    try std.testing.expectError(error.RuntimeError, vm.run_bytecode(&bc, &[_]val.Val{}));
    try std.testing.expectEqualDeep(vm.stack.items, &[_]val.Val{
        vm.get_symbol("+") orelse return error.SymbolNotFound,
        .{ .int = 10 },
        vm.get_symbol("string-length") orelse return error.SymbolNotFound,
        .{ .int = 4 },
    });
    try std.testing.expectEqualDeep(vm.function_frames.items, &[_]FunctionFrame{
        .{ .bytecode = &bc, .stack_start = 0, .bytecode_idx = 6 },
    });
}
