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
    DefLocal,
    DefTop,
    SetTop,
    SetLex,
    Jump,
    JumpIfFalse,
    Env,
    Closure,
}