namespace VM;

public enum OpCode : byte {
    Push,
    Pop,
    Lit,
    PushContinuationForArg,
    PushContinuationForNonTailBody,
    PopContinuation,
    Call,
    Load,
    Local,
    DefLocal, // we don't need this and SetLex
    SetLex,
    Bind,
    BindRest,
    Store,
    Jump,
    JumpIfFalse,
    Env,
    Lambda,
    Sum,
    Product,
    CallCC,
    CallWValues,
    Values,
}