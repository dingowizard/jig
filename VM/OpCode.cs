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
    BindWRest,
    DefLocal,
    DefTop,
    SetTop,
    SetLex,
    Jump,
    JumpIfFalse,
}