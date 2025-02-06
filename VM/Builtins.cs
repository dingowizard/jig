using Jig;

namespace VM;

public static class Builtins {

    public static readonly Template Cons = new Template(
        code: [
            (ulong)OpCode.Cons << 56,
            (ulong)OpCode.PopContinuation << 56,
        ],
        bindings: [],
        lits: []
    );

    public static Template Car = new Template(
        code: [
            (ulong)OpCode.Car << 56,
            (ulong)OpCode.PopContinuation << 56,
        ],
        bindings: [],
        lits: []
    );
    
    public static Template Cdr = new Template(
        code: [
            (ulong)OpCode.Cdr << 56,
            (ulong)OpCode.PopContinuation << 56,
        ],
        bindings: [],
        lits: []
    );
    
    public static Template NullP = new Template(
        code: [
            (ulong)OpCode.NullP << 56,
            (ulong)OpCode.PopContinuation << 56,
        ],
        bindings: [],
        lits: []
    );
    
    public static Template Sum = new Template(
        code: [
            (ulong)OpCode.Sum << 56,
            (ulong)OpCode.PopContinuation << 56,
        ],
        bindings: [],
        lits: []
    );
    
    public static Template ZeroP = new Template(
        code: [
            (ulong)OpCode.ZeroP << 56,
            (ulong)OpCode.PopContinuation << 56,
        ],
        bindings: [],
        lits: []
    );
}