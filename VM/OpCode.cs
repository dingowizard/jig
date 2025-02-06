namespace VM;

public enum OpCode : byte {
    Push,
    Pop,
    Lit,
    PushContinuation,
    PopContinuation,
    Call,
    Load,
    Bind,
    BindRest,
    Store,
    Jump,
    JumpIfFalse,
    Env,
    Lambda,
    Cons,
    Car,
    Cdr,
    NullP,
    Sum,
    ZeroP
}