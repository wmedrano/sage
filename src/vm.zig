const std = @import("std");
const Val = @import("val.zig").Val;
const ValType = @import("val.zig").ValType;
const Function = @import("val.zig").Function;
const bc = @import("bytecode.zig");

const StackFrame = struct {
    bytecode: *const bc.ByteCodeFunc,
    bytecode_idx: usize = 0,
    stack_start: usize = 0,
};

pub const Vm = struct {
    stack: std.ArrayList(Val),
    function_frames: std.ArrayList(StackFrame),
    builtin_functions: []const Function = &builtin_functions,

    pub fn init(alloc: std.mem.Allocator) !Vm {
        const stack = try std.ArrayList(Val).initCapacity(alloc, 1024);
        const function_frames = try std.ArrayList(StackFrame).initCapacity(alloc, 1024);
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

    pub fn run_bytecode(self: *Vm, bytecode: *const bc.ByteCodeFunc) !Val {
        try self.function_frames.append(.{ .bytecode = bytecode });
        while (try self.run_next()) {}
        const ret = self.stack.getLastOrNull() orelse Val{ .int = 0 };
        return try ret.clone(self.allocator());
    }

    pub fn run_next(self: *Vm) !bool {
        if (self.function_frames.items.len == 0) return false;
        const function_frame = &self.function_frames.items[self.function_frames.items.len - 1];
        const instruction = function_frame.bytecode.instructions.items[function_frame.bytecode_idx];
        function_frame.bytecode_idx += 1;
        switch (instruction) {
            bc.ByteCodeType.push_const => {
                const const_idx = instruction.push_const;
                const v = try function_frame.bytecode.constants.items[const_idx].clone(self.allocator());
                try self.stack.append(v);
            },
            bc.ByteCodeType.deref => {
                const val = try switch (self.stack.getLast()) {
                    ValType.symbol => |s| self.get_symbol(s) orelse error.SymbolNotFound,
                    else => return error.ExpectedSymbol,
                };
                const cloned_val = try val.clone(self.allocator());
                self.stack.pop().deinit(self.allocator());
                try self.stack.append(cloned_val);
            },
            bc.ByteCodeType.eval => |n| {
                const function_idx = self.stack.items.len - n;
                const stack = self.stack.items[function_idx + 1 ..];
                switch (self.stack.items[function_idx]) {
                    ValType.function => |f| {
                        const res = try f.function(stack);
                        for (self.stack.items[function_idx..]) |v| v.deinit(self.allocator());
                        try self.stack.resize(function_idx);
                        try self.stack.append(res);
                    },
                    else => return error.ExpectedFunction,
                }
            },
            bc.ByteCodeType.ret => {
                if (function_frame.stack_start >= self.stack.items.len) {
                    try self.stack.append(.{ .int = 0 });
                }
                for (self.stack.items[function_frame.stack_start + 1 ..]) |v| v.deinit(self.allocator());
                try self.stack.resize(function_frame.stack_start + 1);
                _ = self.function_frames.pop();
            },
        }
        return true;
    }

    fn get_symbol(self: *Vm, symbol: []const u8) ?Val {
        for (self.builtin_functions) |*f| {
            if (std.mem.eql(u8, symbol, f.name)) {
                return Val{ .function = f };
            }
        }
        return null;
    }
};

const builtin_functions = [_]Function{
    .{ .name = "+", .function = add_function },
    .{ .name = "string-length", .function = string_length_function },
};

fn string_length_function(args: []Val) !Val {
    if (args.len != 1) {
        return error.RuntimeError;
    }
    switch (args[0]) {
        ValType.string => |s| return .{ .int = @intCast(s.len) },
        else => return error.RuntimeError,
    }
}

fn add_function(args: []Val) !Val {
    var int_sum: i64 = 0;
    var float_sum: f64 = 0.0;
    var has_float = false;
    for (args) |arg| {
        switch (arg) {
            ValType.float => |f| {
                has_float = true;
                float_sum += f;
            },
            ValType.int => |i| int_sum += i,
            else => return error.RuntimeError,
        }
    }
    if (has_float) {
        return error.NotImplemented;
    }
    return .{ .int = int_sum };
}
