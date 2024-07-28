const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const ast = @import("ast.zig");
const ReferenceMarker = @import("heap.zig").ReferenceMarker;
const Heap = @import("heap.zig").Heap;
const Val = @import("val.zig").Val;
const ByteCodeFunc = @import("bytecode.zig").ByteCodeFunc;

/// Contains details for the current function call.
const FunctionFrame = struct {
    /// The bytecode that is being executed.
    bytecode: *const ByteCodeFunc,
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

pub const DefaultDebugger = struct {
    pub inline fn nextInstruction(_: *DefaultDebugger, _: *Vm) void {}
};

pub const Vm = struct {
    /// Contains all stack variables in the Vm. A clean execution should start and end with an empty
    /// stack. If an error occurs, the stack is preserved for further debugging.
    stack: std.ArrayListUnmanaged(Val),
    /// Contains all the function calls with the last element containing the current function call.
    function_frames: std.ArrayListUnmanaged(FunctionFrame),
    /// Contains all functions that are built in.
    builtin_functions: []const Val.Function = &@import("builtin_functions.zig").builtin_functions,
    /// Contains all defined values.
    values: std.StringHashMapUnmanaged(Val),
    /// Contains all data on the heap.
    heap: Heap,
    /// Intermediate datastructure for keeping track of references.
    references: ReferenceMarker,

    /// Initialize a new VM with the given allocator.
    pub fn init(alloc: std.mem.Allocator) VmError!Vm {
        const heap = Heap.init(alloc);
        const stack = try std.ArrayListUnmanaged(Val).initCapacity(alloc, 1024);
        const function_frames = try std.ArrayListUnmanaged(FunctionFrame).initCapacity(alloc, 1024);
        return .{
            .stack = stack,
            .function_frames = function_frames,
            .values = std.StringHashMapUnmanaged(Val){},
            .heap = heap,
            .references = ReferenceMarker.init(alloc),
        };
    }

    /// Deinitialize the VM. This deallocates the VM's allocated memory.
    pub fn deinit(self: *Vm) void {
        self.stack.deinit(self.allocator());
        self.function_frames.deinit(self.allocator());
        self.values.deinit(self.allocator());
        self.heap.deinit();
    }

    /// Get the allocator used by the Vm.
    pub inline fn allocator(self: *Vm) std.mem.Allocator {
        return self.heap.allocator;
    }

    pub fn defineVal(self: *Vm, symbol: []const u8, val: Val) !void {
        const symbol_val = try self.heap.allocGlobalSymbol(symbol);
        const symbol_str = switch (symbol_val) {
            .symbol => |s| s.data,
            else => unreachable,
        };
        try self.values.put(self.allocator(), symbol_str, val);
    }

    pub fn currentFunctionFrame(self: *const Vm) ?*FunctionFrame {
        const function_frames_count = self.function_frames.items.len;
        if (function_frames_count == 0) return null;
        return &self.function_frames.items[function_frames_count - 1];
    }

    /// Run the bytecode with the given args. On successful execution, a Val is returned and the
    /// stack is reset. On error, the stack will remain as it was in the last error. clearStack may
    /// be called to continue to reuse the Vm.
    ///
    /// Note that the returned val may become unavailable after calling runGc.
    pub fn runWithDebugger(self: *Vm, bc: *const ByteCodeFunc, args: []Val, debugger: anytype) VmError!Val {
        if (args.len > 0) {
            return VmError.NotImplemented;
        }
        if (self.stack.items.len != 0) {
            return VmError.CorruptStack;
        }
        try self.stack.appendSlice(self.allocator(), args);
        try self.function_frames.append(self.allocator(), .{ .bytecode = bc, .stack_start = 0 });
        while (try self.runNext(debugger)) {}
        const ret = self.stack.popOrNull() orelse .void;
        self.clearStack();
        return ret;
    }

    /// Run the bytecode with the given args. On successful execution, a Val is returned and the
    /// stack is reset. On error, the stack will remain as it was in the last error. clearStack may
    /// be called to continue to reuse the Vm.
    ///
    /// Note that the returned val may become unavailable after calling runGc.
    pub fn runBytecode(self: *Vm, bc: *const ByteCodeFunc, args: []Val) VmError!Val {
        var debugger = DefaultDebugger{};
        return self.runWithDebugger(bc, args, &debugger);
    }

    pub fn runGc(self: *Vm) !void {
        self.references.reset();
        try self.references.markVals(self.stack.items);
        var values_iter = self.values.valueIterator();
        while (values_iter.next()) |v| {
            try self.references.markVal(v.*);
        }
        self.heap.removeGarbage(&self.references);
    }

    /// Clear the stack of all its contents.
    fn clearStack(self: *Vm) void {
        self.stack.clearRetainingCapacity();
    }

    /// Get the given symbol or null if it is not defined in the global scope.
    fn getSymbol(self: *Vm, symbol: []const u8) ?Val {
        if (self.values.get(symbol)) |v| {
            return v;
        }
        for (self.builtin_functions) |*f| {
            if (std.mem.eql(u8, symbol, f.name)) {
                return Val{ .function = f };
            }
        }
        return null;
    }

    /// Run the next instruction and return if the Vm should continue executing.
    fn runNext(self: *Vm, debugger: anytype) VmError!bool {
        if (self.function_frames.items.len == 0) return false;
        debugger.nextInstruction(self);
        const function_frame = &self.function_frames.items[self.function_frames.items.len - 1];
        const instruction = function_frame.bytecode.instructions[function_frame.bytecode_idx];
        switch (instruction) {
            .push_const => |i| try self.executePushConst(function_frame.bytecode, i),
            .deref => |i| try self.executeDeref(function_frame.bytecode, i),
            .get_arg => |n| try self.executeGetArg(n),
            .eval => |n| try self.executeEval(n),
            .jump => |n| self.executeJump(n),
            .jump_if => |n| self.executeJumpIf(n),
            .ret => try self.executeRet(),
        }
        function_frame.bytecode_idx += 1;
        return true;
    }

    /// Execute the push_const instruction.
    fn executePushConst(self: *Vm, bc: *const ByteCodeFunc, const_idx: usize) VmError!void {
        const v = bc.constants[const_idx];
        try self.stack.append(self.allocator(), v);
    }

    /// Execute the deref instruction.
    fn executeDeref(self: *Vm, bc: *const ByteCodeFunc, idx: usize) VmError!void {
        const v = switch (bc.constants[idx]) {
            Val.Type.symbol => |s| self.getSymbol(s.data) orelse return VmError.UndefinedSymbol,
            else => return VmError.WrongType,
        };
        try self.stack.append(self.allocator(), v);
    }

    fn executeGetArg(self: *Vm, n: usize) VmError!void {
        const stack_start = self.function_frames.items[self.function_frames.items.len - 1].stack_start;
        const idx = stack_start + n;
        try self.stack.append(self.allocator(), self.stack.items[idx]);
    }

    /// Execute the eval instruction.
    fn executeEval(self: *Vm, n: usize) VmError!void {
        const function_idx = self.stack.items.len - n;
        const stack = self.stack.items[function_idx + 1 ..];
        const call_val = self.stack.items[function_idx];
        switch (call_val) {
            .function => |f| {
                switch (f.function) {
                    .native => |nf| {
                        const res = try nf(self, stack);
                        try self.stack.resize(self.allocator(), function_idx);
                        try self.stack.append(self.allocator(), res);
                    },
                    .bytecode => |*b| {
                        try self.function_frames.append(self.allocator(), .{
                            .bytecode = b,
                            .stack_start = function_idx + 1,
                        });
                    },
                }
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
            try self.stack.append(self.allocator(), .void);
            return;
        }
        const ret = self.stack.pop();
        try self.stack.resize(self.allocator(), function_frame.stack_start);
        _ = self.stack.popOrNull();
        self.stack.appendAssumeCapacity(ret);
    }
};

test "expression can eval" {
    var vm = try Vm.init(std.testing.allocator);
    defer vm.deinit();
    var bc = try ByteCodeFunc.initStrExpr("(+ (string-length \"four\") -5)", &vm.heap);
    defer bc.deinit(std.testing.allocator);

    const actual = try vm.runBytecode(&bc, &[_]Val{});
    try std.testing.expectEqualDeep(
        Val{ .int = -1 },
        actual,
    );
}

test "can eval fibonacchi" {
    var vm = try Vm.init(std.testing.allocator);
    defer vm.deinit();

    var bc1 = try ByteCodeFunc.initStrExpr("(define fib (lambda (n) (if (< n 1) 0 (if (< n 3) 1 (+ (fib (- n 1)) (fib (- n 2)))))))", &vm.heap);
    defer bc1.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(.void, try vm.runBytecode(&bc1, &[_]Val{}));

    var bc2 = try ByteCodeFunc.initStrExpr("(fib 10)", &vm.heap);
    defer bc2.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(Val{ .int = 55 }, try vm.runBytecode(&bc2, &[_]Val{}));

    var bc3 = try ByteCodeFunc.initStrExpr("(fib 1)", &vm.heap);
    defer bc3.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(Val{ .int = 1 }, try vm.runBytecode(&bc3, &[_]Val{}));

    var bc4 = try ByteCodeFunc.initStrExpr("(fib 0)", &vm.heap);
    defer bc4.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(Val{ .int = 0 }, try vm.runBytecode(&bc4, &[_]Val{}));
}

test "successful expression clears stack" {
    var vm = try Vm.init(std.testing.allocator);
    defer vm.deinit();
    var bc = try ByteCodeFunc.initStrExpr("(+ 1 2 3)", &vm.heap);
    defer bc.deinit(std.testing.allocator);

    _ = try vm.runBytecode(&bc, &[_]Val{});

    try std.testing.expectEqualDeep(vm.stack.items, &[_]Val{});
}

test "wrong args halts VM and maintains VM state" {
    var vm = try Vm.init(std.testing.allocator);
    defer vm.deinit();
    var bc = try ByteCodeFunc.initStrExpr("(+ 10 (string-length 4))", &vm.heap);
    defer bc.deinit(std.testing.allocator);

    try std.testing.expectError(error.RuntimeError, vm.runBytecode(&bc, &[_]Val{}));
    try std.testing.expectEqualDeep(vm.stack.items, &[_]Val{
        vm.getSymbol("+") orelse return error.SymbolNotFound,
        .{ .int = 10 },
        vm.getSymbol("string-length") orelse return error.SymbolNotFound,
        .{ .int = 4 },
    });
    try std.testing.expectEqualDeep(vm.function_frames.items, &[_]FunctionFrame{
        .{ .bytecode = &bc, .stack_start = 0, .bytecode_idx = 4 },
    });
}

test "if expression with true pred returns true branch" {
    var vm = try Vm.init(std.testing.allocator);
    defer vm.deinit();
    var bc = try ByteCodeFunc.initStrExpr("(if true 1 2)", &vm.heap);
    defer bc.deinit(std.testing.allocator);

    const v = try vm.runBytecode(&bc, &[_]Val{});
    try std.testing.expectEqualDeep(Val{ .int = 1 }, v);
}

test "if expression with false pred returns false branch" {
    var vm = try Vm.init(std.testing.allocator);
    defer vm.deinit();
    var bc = try ByteCodeFunc.initStrExpr("(if false 1 2)", &vm.heap);
    defer bc.deinit(std.testing.allocator);

    const v = try vm.runBytecode(&bc, &[_]Val{});
    try std.testing.expectEqualDeep(Val{ .int = 2 }, v);
}

test "if expression with false pred and empty false branch returns void" {
    var vm = try Vm.init(std.testing.allocator);
    defer vm.deinit();
    var bc = try ByteCodeFunc.initStrExpr("(if false 1)", &vm.heap);
    defer bc.deinit(std.testing.allocator);

    const v = try vm.runBytecode(&bc, &[_]Val{});
    try std.testing.expectEqualDeep(.void, v);
}

test "lambda can eval" {
    var vm = try Vm.init(std.testing.allocator);
    defer vm.deinit();
    var bc = try ByteCodeFunc.initStrExpr("((lambda (a b c) (+ 1 a b c)) 2 3 4)", &vm.heap);
    defer bc.deinit(std.testing.allocator);

    const actual = try vm.runBytecode(&bc, &[_]Val{});
    try std.testing.expectEqualDeep(
        Val{ .int = 10 },
        actual,
    );
}
