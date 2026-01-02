namespace VM;

public enum OpCode : byte {
    Push,
    Pop,
    Lit,
    PushContinuationForArg,
    PushContinuationForNonTailBody,
    PopContinuation,
    Call,
    Var,
    SetVar,
    Arg,
    SetArg,
    Jump,
    JumpIfFalse,
    Env,
    Lambda,
    Add,
    Product,
    CallCC,
    CallWValues,
    PushWinder,
    PushContinuationForBodyThunk,
    PopWinder,
    PushFP,
    SPToFP,
    PopFP,
    ArgToArgs, // takes list arg from stack and pushes each element onto stack. TODO: yuck
    Halt,
    // Experimental:
    Car,
}