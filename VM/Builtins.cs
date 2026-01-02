namespace VM;

public static class Builtins {

    public static readonly Template Sum = new (
        1,
        code: [
            (ulong)OpCode.Add << 56,
            (ulong)OpCode.PopContinuation << 56,
        ],
        vars: [],
        lits: [],
        0,
        true
    );
    
    public static readonly Template BinOpPlus = new (
        2,
        code: [
            (ulong)OpCode.Add << 56,
            (ulong)OpCode.PopContinuation << 56,
        ],
        vars: [],
        lits: [],
        2,
        false
    );
    
    public static readonly Template Product = new (
        1,
        code: [
            (ulong)OpCode.Product << 56,
            (ulong)OpCode.PopContinuation << 56,
        ],
        vars: [],
        lits: [],
        0,
        true
    );

    // public static readonly Template Car2 = new (
    //     1,
    //     [
    //         // (pair? arg0)
    //         (ulong)OpCode.PushContinuationForArg + (ulong)6, // address of jump if false
    //         ((ulong)OpCode.Arg << 56) + (ulong)0, // arg0 -> VAL
    //         ((ulong)OpCode.Push << 56),
    //         ((ulong)OpCode.Var << 56) + (ulong)0, // pair?
    //         ((ulong)OpCode.Push << 56),
    //         ((ulong)OpCode.Call << 56),
    //         ((ulong)OpCode.JumpIfFalse << 56) + (ulong)9,
    //         // then branch
    //         ((ulong)OpCode.Car << 56),
    //         ((ulong)OpCode.PopContinuation), // return
    //         // else branch
    //         ((ulong) OpCode.Lit << 56) + (ulong)0, // error message
    //         ((ulong)OpCode.Push << 56),
    //         ((ulong)OpCode.Lit << 56), + (ulong)1, // symbol car
    //         ((ulong)OpCode.Push << 56),
    //         ((ulong)OpCode.Var << 56) + (ulong)0, // error
    //         ((ulong)OpCode.Push << 56),
    //         ((ulong)OpCode.Call << 56),
    //     ],
    //     [
    //         Library.Core.FindParameter("pair?"),
    //         Library.Core.FindParameter("error"),
    //     ],
    //     [
    //         new Jig.String("expected argument to be a pair"),
    //         new Symbol("car"),
    //     ],
    //     1,
    //     false
    // );


    public static readonly Template Apply = new (
        numVarsForScope: 2,
        code: [
            (ulong)OpCode.Arg << 56, // store proc in VAL
            ((ulong)OpCode.ArgToArgs << 56) + 1, // TODO: could we remove ArgToArgs and replace it with an APPLY instruction?
            (ulong)OpCode.Push << 56, // put proc back on stack
            (ulong)OpCode.Call << 56,
            (ulong)OpCode.PopContinuation << 56,
        
        ],
        vars: [],
        lits: [],
        2,
        false
    );
    
    public static readonly Template CallWithValues = new (
        // TODO: can this by re-done more in the style of DynamicWind?
        2,
        code: [
            (ulong)OpCode.CallWValues << 56,
            (ulong)OpCode.PopContinuation << 56,
        ],
        vars: [],
        lits: [],
        2,
        false
    );
    
    public static readonly Template CallCC = new (
        // TODO: this could be a primitive?
        1,
        code: [
            (ulong)OpCode.CallCC << 56,
            (ulong)OpCode.PopContinuation << 56,
        ],
        vars: [],
        lits: [],
        1,
        false
        );

    public static readonly Template DynamicWind = new(
        // TODO: you need to delete the bind instr
        // but then all the addresses will be too high by one
        numVarsForScope: 3,
        code: [
            ((ulong)OpCode.PushContinuationForNonTailBody << 56) + 4,
            ((ulong)OpCode.Arg << 56), // + 0 + 0 omitted.
            ((ulong)OpCode.Push << 56),
            ((ulong)OpCode.Call << 56),
            // push winders
            ((ulong)OpCode.Arg << 56) + 2, // + 0 + 2 
            ((ulong)OpCode.Push << 56),
            ((ulong)OpCode.Arg << 56), // + 0 + 0 omitted.
            ((ulong)OpCode.Push << 56),
            ((ulong)OpCode.PushWinder << 56),
            
            // results of in-thunk were discarded by ContinuationForNonTailBody
            // So, FP = SP and FP is what we will want  FP to be set to after results of out-thunk have been discarded
            ((ulong)OpCode.PushContinuationForBodyThunk << 56) + 13, // this will need to set FP to SP
                                                                     // in order to save result on stack
                                                                     // or we need new instruction to do same below
                                                                    // before or after popwinder instr
            ((ulong)OpCode.Arg << 56) + 1, // + 0 + 1 omitted.
            ((ulong)OpCode.Push << 56),
            ((ulong)OpCode.Call << 56),
            (ulong)OpCode.PopWinder << 56,
            (ulong)OpCode.PushFP << 56, // save the frame pointer on the stack
            (ulong)OpCode.SPToFP << 56, // set current fp to sp
            ((ulong)OpCode.PushContinuationForNonTailBody << 56) + 20,
            ((ulong)OpCode.Arg << 56) + 2, // + 0 + 2
            ((ulong)OpCode.Push << 56),
            ((ulong)OpCode.Call << 56),
            // Now out-thunk ran and results were discarded.
            // we need to restore results of body-thunk
            // SP should be one ahead of where SP is stored on stack
            ((ulong)OpCode.PopFP <<56),
            ((ulong)OpCode.PopContinuation << 56),
        ],
        vars: [],
        lits: [],
        requiredParameterCount:3,
        hasRestParameter: false
        );
}