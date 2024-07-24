const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const ast = @import("ast.zig");
const Val = @import("val.zig").Val;
const bytecode = @import("bytecode.zig");

/// Contains details for the current function call.
const FunctionFrame = struct {
    /// The bytecode that is being executed.
    bytecode: *const bytecode.ByteCodeFunc,
    /// The index to the start of the current function's stack.
    stack_start: usize,
    /// The index to the next instruction to execute within bytecode.
    bytecode_idx: usize = 0,
};

/// Errors that occur when running the VM.
pub const VmError = std.mem.Allocator.Error || error{
    /// Something unexpected happened.
    RuntimeError,
    /// The stack is not as expected. Usually happens when continuing to run the Vm after a failure.
    CorruptStack,
    /// A function call expected a certain type but got another.
    WrongType,
    /// A symbol was not defined.
    UndefinedSymbol,
    /// The functionality is not implemented.
    NotImplemented,
};

pub const Vm = struct {
    /// Contains all stack variables in the Vm. A clean execution should start and end with an empty
    /// stack. If an error occurs, the stack is preserved for further debugging.
    stack: std.ArrayList(Val),
    /// Contains all the function calls with the last element containing the current function call.
    function_frames: std.ArrayListUnmanaged(FunctionFrame),
    /// Contains all functions that are built in.
    builtin_functions: []const Val.Function = &builtin_functions,

    /// Initialize a new VM with the given allocator.
    pub fn init(alloc: std.mem.Allocator) VmError!Vm {
        const stack = try std.ArrayList(Val).initCapacity(alloc, 1024);
        const function_frames = try std.ArrayListUnmanaged(FunctionFrame).initCapacity(alloc, 1024);
        return .{
            .stack = stack,
            .function_frames = function_frames,
        };
    }

    /// Deinitialize the VM. This deallocates the VM's allocated memory.
    pub fn deinit(self: *Vm) void {
        for (self.stack.items) |x| x.deinit(self.allocator());
        self.stack.deinit();
        self.function_frames.deinit(self.allocator());
    }

    /// Get the allocator used by the Vm.
    pub fn allocator(self: *Vm) std.mem.Allocator {
        return self.stack.allocator;
    }

    /// Run the bytecode with the given args. On successful execution, a Val is returned and the
    /// stack is reset. On error, the stack will remain as it was in the last error. clearStack may
    /// be called to continue to reuse the Vm.
    pub fn runBytecode(self: *Vm, bc: *const bytecode.ByteCodeFunc, args: []Val) VmError!Val {
        if (args.len > 0) {
            return VmError.NotImplemented;
        }
        if (self.stack.items.len != 0) {
            return VmError.CorruptStack;
        }
        for (args) |arg| {
            try self.stack.append(try arg.clone(self.allocator()));
        }
        try self.function_frames.append(self.allocator(), .{ .bytecode = bc, .stack_start = 0 });
        while (try self.runNext()) {}
        const ret = self.stack.popOrNull() orelse Val{ .int = 0 };
        self.clearStack();
        return ret;
    }

    /// Clear the stack of all its contents.
    fn clearStack(self: *Vm) void {
        for (self.stack.items) |v| v.deinit(self.allocator());
        self.stack.clearRetainingCapacity();
    }

    /// Get the given symbol or null if it is not defined in the global scope.
    fn getSymbol(self: *Vm, symbol: []const u8) ?Val {
        for (self.builtin_functions) |*f| {
            if (std.mem.eql(u8, symbol, f.name)) {
                return Val{ .function = f };
            }
        }
        return null;
    }

    /// Run the next instruction and return if the Vm should continue executing.
    fn runNext(self: *Vm) VmError!bool {
        if (self.function_frames.items.len == 0) return false;
        const function_frame = &self.function_frames.items[self.function_frames.items.len - 1];
        const instruction = function_frame.bytecode.instructions.items[function_frame.bytecode_idx];
        switch (instruction) {
            .push_const => |i| try self.executePushConst(function_frame.bytecode, i),
            .deref => try self.executeDeref(),
            .eval => |n| try self.executeEval(n),
            .jump => |n| self.executeJump(n),
            .jump_if => |n| self.executeJumpIf(n),
            .ret => try self.executeRet(),
        }
        function_frame.bytecode_idx += 1;
        return true;
    }

    /// Execute the push_const instruction.
    fn executePushConst(self: *Vm, bc: *const bytecode.ByteCodeFunc, const_idx: usize) VmError!void {
        const v = try bc.constants.items[const_idx].clone(self.allocator());
        try self.stack.append(v);
    }

    /// Execute the deref instruction.
    fn executeDeref(self: *Vm) VmError!void {
        const v = try switch (self.stack.getLast()) {
            Val.Type.symbol => |s| self.getSymbol(s) orelse VmError.UndefinedSymbol,
            else => return VmError.WrongType,
        };
        const cloned_val = try v.clone(self.allocator());
        self.stack.pop().deinit(self.allocator());
        try self.stack.append(cloned_val);
    }

    /// Execute the eval instruction.
    fn executeEval(self: *Vm, n: usize) VmError!void {
        const function_idx = self.stack.items.len - n;
        const stack = self.stack.items[function_idx + 1 ..];
        switch (self.stack.items[function_idx]) {
            Val.Type.function => |f| {
                const res = try f.function(stack);
                for (self.stack.items[function_idx..]) |v| v.deinit(self.allocator());
                try self.stack.resize(function_idx);
                try self.stack.append(res);
            },
            else => return VmError.WrongType,
        }
    }

    /// Execute the jump instruction.
    fn executeJump(self: *Vm, n: usize) void {
        self.function_frames.items[self.function_frames.items.len - 1].bytecode_idx += n;
    }

    /// Execute the jump_if instruction.
    fn executeJumpIf(self: *Vm, n: usize) void {
        const pred_res = self.stack.pop();
        if (pred_res.isTruthy()) {
            self.executeJump(n);
        }
    }

    /// Execute the return instruction.
    fn executeRet(self: *Vm) VmError!void {
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

const builtin_functions = [_]Val.Function{
    .{ .name = "+", .function = addFunction },
    .{ .name = "string-length", .function = stringLengthFunction },
};

fn stringLengthFunction(args: []Val) !Val {
    if (args.len != 1) {
        return error.RuntimeError;
    }
    switch (args[0]) {
        Val.Type.string => |s| return .{ .int = @intCast(s.len) },
        else => return error.RuntimeError,
    }
}

fn addFunction(args: []Val) !Val {
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
        return error.NotImplemented;
    }
    return .{ .int = int_sum };
}

test "expression can eval" {
    var bc = try bytecode.ByteCodeFunc.initStrExpr("(+ (string-length \"four\") -5)", std.testing.allocator);
    defer bc.deinit();

    var vm = try Vm.init(std.testing.allocator);
    defer vm.deinit();
    const actual = try vm.runBytecode(&bc, &[_]Val{});
    defer actual.deinit(vm.allocator());
    try std.testing.expectEqualDeep(
        Val{ .int = -1 },
        actual,
    );
}

test "successful expression clears stack" {
    const src = "(+ 1 2 3)";
    const asts = try ast.AstCollection.initWithStr(src, std.testing.allocator);
    defer asts.deinit();

    var bc = try bytecode.ByteCodeFunc.initStrExpr("(+ 1 2 3)", std.testing.allocator);
    defer bc.deinit();

    var vm = try Vm.init(std.testing.allocator);
    defer vm.deinit();
    var v = try vm.runBytecode(&bc, &[_]Val{});
    defer v.deinit(vm.allocator());

    try std.testing.expectEqualDeep(vm.stack.items, &[_]Val{});
}

test "wrong args halts VM and maintains VM state" {
    var bc = try bytecode.ByteCodeFunc.initStrExpr("(+ 10 (string-length 4))", std.testing.allocator);
    defer bc.deinit();

    var vm = try Vm.init(std.testing.allocator);
    defer vm.deinit();
    try std.testing.expectError(error.RuntimeError, vm.runBytecode(&bc, &[_]Val{}));
    try std.testing.expectEqualDeep(vm.stack.items, &[_]Val{
        vm.getSymbol("+") orelse return error.SymbolNotFound,
        .{ .int = 10 },
        vm.getSymbol("string-length") orelse return error.SymbolNotFound,
        .{ .int = 4 },
    });
    try std.testing.expectEqualDeep(vm.function_frames.items, &[_]FunctionFrame{
        .{ .bytecode = &bc, .stack_start = 0, .bytecode_idx = 6 },
    });
}
