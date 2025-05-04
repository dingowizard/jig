namespace VM;

public static class Builtins {

    public static readonly Template Sum = new Template(
        0, // NOTE: the only reason this works is that the Sum instruction does not use 
                          // a parameter -- it operates on the stack directly
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

    public static readonly Template DynamicWind2 = new(
        numVarsForScope: 3,
        code: [
            ((ulong)OpCode.Bind << 56) + 3, //0
            ((ulong)OpCode.PushContinuationForNonTailBody << 56) + 5,
            ((ulong)OpCode.Lex << 56), // + 0 + 0 omitted.
            ((ulong)OpCode.Push << 56),
            ((ulong)OpCode.Call << 56), //4
            // push winders
            ((ulong)OpCode.Lex << 56) + 2, // + 0 + 2 
            ((ulong)OpCode.Push << 56),
            ((ulong)OpCode.Lex << 56), // + 0 + 0 omitted.
            ((ulong)OpCode.Push << 56), //8
            ((ulong)OpCode.PushWinder << 56),
            
            // results of in-thunk were discarded by ContinuationForNonTailBody
            // So, FP = SP and FP is what we will want  FP to be set to after results of out-thunk have been discarded
            ((ulong)OpCode.PushContinuationForBodyThunk << 56) + 14, // this will need to set FP to SP
                                                                     // in order to save result on stack
                                                                     // or we need new instruction to do same below
                                                                    // before or after popwinder instr
            ((ulong)OpCode.Lex << 56) + 1, // + 0 + 1 omitted.
            ((ulong)OpCode.Push << 56), // 12
            ((ulong)OpCode.Call << 56),
            (ulong)OpCode.PopWinder << 56,
            (ulong)OpCode.PushFP << 56, // save the frame pointer on the stack
            (ulong)OpCode.SPToFP << 56, // set current fp to sp
            ((ulong)OpCode.PushContinuationForNonTailBody << 56) + 21, // 17
            ((ulong)OpCode.Lex << 56) + 2, // + 0 + 2
            ((ulong)OpCode.Push << 56),
            ((ulong)OpCode.Call << 56),
            // Now out-thunk ran and results were discarded.
            // we need to restore results of body-thunk
            // SP should be one ahead of where SP is stored on stack
            ((ulong)OpCode.PopFP <<56),
            ((ulong)OpCode.PopContinuation << 56)
        ],
        bindings: [],
        lits: [],
        requiredParameterCount:3,
        hasRestParameter: false
        );
}