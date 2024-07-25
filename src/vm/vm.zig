const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const ast = @import("ast.zig");
const ReferenceMarker = @import("heap.zig").ReferenceMarker;
const Heap = @import("heap.zig").Heap;
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
    builtin_functions: []const Val.Function = &@import("builtin_functions.zig").builtin_functions,
    /// Contains all data on the heap.
    heap: Heap,
    /// Intermediate datastructure for keeping track of references.
    references: ReferenceMarker,

    /// Initialize a new VM with the given allocator.
    pub fn init(alloc: std.mem.Allocator) VmError!Vm {
        const heap = Heap.init(alloc);
        const stack = try std.ArrayList(Val).initCapacity(alloc, 1024);
        const function_frames = try std.ArrayListUnmanaged(FunctionFrame).initCapacity(alloc, 1024);
        return .{
            .stack = stack,
            .function_frames = function_frames,
            .heap = heap,
            .references = ReferenceMarker.init(alloc),
        };
    }

    /// Deinitialize the VM. This deallocates the VM's allocated memory.
    pub fn deinit(self: *Vm) void {
        self.stack.deinit();
        self.function_frames.deinit(self.allocator());
        self.heap.deinit();
    }

    /// Get the allocator used by the Vm.
    pub fn allocator(self: *Vm) std.mem.Allocator {
        return self.stack.allocator;
    }

    /// Run the bytecode with the given args. On successful execution, a Val is returned and the
    /// stack is reset. On error, the stack will remain as it was in the last error. clearStack may
    /// be called to continue to reuse the Vm.
    ///
    /// Note that the returned val may become unavailable after calling runGc.
    pub fn runBytecode(self: *Vm, bc: *const bytecode.ByteCodeFunc, args: []Val) VmError!Val {
        if (args.len > 0) {
            return VmError.NotImplemented;
        }
        if (self.stack.items.len != 0) {
            return VmError.CorruptStack;
        }
        try self.stack.appendSlice(args);
        try self.function_frames.append(self.allocator(), .{ .bytecode = bc, .stack_start = 0 });
        while (try self.runNext()) {}
        const ret = self.stack.popOrNull() orelse .void;
        self.clearStack();
        return ret;
    }

    pub fn runGc(self: *Vm) !void {
        self.references.reset();
        try self.references.markVals(self.stack.items);
        self.heap.removeGarbage(&self.references);
    }

    /// Clear the stack of all its contents.
    fn clearStack(self: *Vm) void {
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
        const v = bc.constants.items[const_idx];
        try self.stack.append(v);
    }

    /// Execute the deref instruction.
    fn executeDeref(self: *Vm) VmError!void {
        const v = switch (self.stack.getLast()) {
            Val.Type.symbol => |s| self.getSymbol(s.data) orelse return VmError.UndefinedSymbol,
            else => return VmError.WrongType,
        };
        self.stack.items[self.stack.items.len - 1] = v;
    }

    /// Execute the eval instruction.
    fn executeEval(self: *Vm, n: usize) VmError!void {
        const function_idx = self.stack.items.len - n;
        const stack = self.stack.items[function_idx + 1 ..];
        const val = self.stack.items[function_idx];
        switch (val) {
            Val.Type.function => |f| {
                const res = try f.function(stack);
                try self.stack.resize(function_idx);
                try self.stack.append(res);
            },
            else => {
                return VmError.WrongType;
            },
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
        try self.stack.resize(function_frame.stack_start);
        try self.stack.append(ret);
    }
};

test "expression can eval" {
    var vm = try Vm.init(std.testing.allocator);
    defer vm.deinit();
    var bc = try bytecode.ByteCodeFunc.initStrExpr("(+ (string-length \"four\") -5)", &vm.heap);
    defer bc.deinit();

    const actual = try vm.runBytecode(&bc, &[_]Val{});
    try std.testing.expectEqualDeep(
        Val{ .int = -1 },
        actual,
    );
}

test "successful expression clears stack" {
    var vm = try Vm.init(std.testing.allocator);
    defer vm.deinit();
    var bc = try bytecode.ByteCodeFunc.initStrExpr("(+ 1 2 3)", &vm.heap);
    defer bc.deinit();

    _ = try vm.runBytecode(&bc, &[_]Val{});

    try std.testing.expectEqualDeep(vm.stack.items, &[_]Val{});
}

test "wrong args halts VM and maintains VM state" {
    var vm = try Vm.init(std.testing.allocator);
    defer vm.deinit();
    var bc = try bytecode.ByteCodeFunc.initStrExpr("(+ 10 (string-length 4))", &vm.heap);
    defer bc.deinit();

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

test "if expression with true pred returns true branch" {
    var vm = try Vm.init(std.testing.allocator);
    defer vm.deinit();
    var bc = try bytecode.ByteCodeFunc.initStrExpr("(if true 1 2)", &vm.heap);
    defer bc.deinit();

    const v = try vm.runBytecode(&bc, &[_]Val{});
    try std.testing.expectEqualDeep(Val{ .int = 1 }, v);
}

test "if expression with false pred returns false branch" {
    var vm = try Vm.init(std.testing.allocator);
    defer vm.deinit();
    var bc = try bytecode.ByteCodeFunc.initStrExpr("(if false 1 2)", &vm.heap);
    defer bc.deinit();

    const v = try vm.runBytecode(&bc, &[_]Val{});
    try std.testing.expectEqualDeep(Val{ .int = 2 }, v);
}

test "if expression with false pred and empty false branch returns void" {
    var vm = try Vm.init(std.testing.allocator);
    defer vm.deinit();
    var bc = try bytecode.ByteCodeFunc.initStrExpr("(if false 1)", &vm.heap);
    defer bc.deinit();

    const v = try vm.runBytecode(&bc, &[_]Val{});
    try std.testing.expectEqualDeep(.void, v);
}
