const std = @import("std");
const Vm = @import("vm/vm.zig").Vm;

pub const StepInspector = struct {
    instruction_count: usize = 0,

    pub fn nextInstruction(self: *StepInspector, vm: *Vm) void {
        self.instruction_count += 1;
        const frame = vm.currentFunctionFrame().?;
        std.debug.print("----------------------------------------\n", .{});
        std.debug.print("Cycle: {any}\n", .{self.instruction_count});
        std.debug.print("Stack: {any}\n", .{vm.stack.items[frame.stack_start..]});
        std.debug.print("FunctionFrame: {any}\n", .{vm.function_frames.items.len});
        std.debug.print("Instruction: {any}\n", .{frame.bytecode.instructions.items[frame.bytecode_idx]});
        std.debug.print("Consts: {any}\n", .{frame.bytecode.constants.items});
    }
};
