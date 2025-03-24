namespace VM;

public static class Builtins {

    public static readonly Template Sum = new Template(
        0,
        code: [
            (ulong)OpCode.Sum << 56,
            (ulong)OpCode.PopContinuation << 56,
        ],
        bindings: [],
        lits: [],
        0,
        true
    );
    
    public static readonly Template Product = new Template(
        0,
        code: [
            (ulong)OpCode.Product << 56,
            (ulong)OpCode.PopContinuation << 56,
        ],
        bindings: [],
        lits: [],
        0,
        true
    );

    public static Template Values = new Template(
        0,
        code: [
            (ulong)OpCode.Values << 56,
            (ulong)OpCode.PopContinuation << 56,
        ],
        bindings: [],
        lits: [],
        0,
        true
    );
    
    public static Template CallWithValues = new Template(
        0,
        code: [
            (ulong)OpCode.CallWValues << 56,
            (ulong)OpCode.PopContinuation << 56,
        ],
        bindings: [],
        lits: [],
        2,
        false
    );
    
    public static readonly Template CallCC = new Template(
        0,
        code: [
            (ulong)OpCode.CallCC << 56,
            (ulong)OpCode.PopContinuation << 56,
        ],
        bindings: [],
        lits: [],
        1,
        false
        );
}