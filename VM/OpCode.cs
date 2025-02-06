namespace VM;

public enum OpCode : byte {
    Push,
    Pop,
    Lit,
    PushContinuation,
    PopContinuation,
    Call,
    Top,
    LexVar,
    Bind,
    BindRest,
    Def,
    SetTop,
    SetLex,
    Jump,
    JumpIfFalse,
    Env,
    Closure,
    Cons,
    Car,
    Cdr,
    NullP,
    Sum,
    ZeroP
}