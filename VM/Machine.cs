using System.Diagnostics;
using Jig;
namespace VM;

public class Machine {

    internal ulong IR;
    
    internal ulong PC;

    internal Form VAL = Form.Void;

    internal Template Template;

    internal List EvalStack = Jig.List.Null;

    internal Continuation CONT;

    internal Environment ENVT;

    private Form Pop() {
        if (EvalStack is not Jig.List.NonEmpty proper) throw new Exception("stack is empty");
        Form result = (Form)proper.Car;
        EvalStack = proper.Rest;
        return result;
    }

    private void Push(Form val) {
        EvalStack = Jig.List.Cons(val, EvalStack);
    }

    private void ClearStack() {
        EvalStack = Jig.List.Null;
    }

    

    public Jig.Form Run() {
        while (true) {
            // Fetch
            Debug.Assert(PC < (ulong)Template.Code.Length,
                $"PC was {PC} and Template length was {Template.Code.Length}");
            IR = Template.Code[PC];
            PC++;
            // Execute
            OpCode opCode = (OpCode)(IR >> 56);
            switch (opCode) {
                case OpCode.Push:
                    Push(VAL);
                    continue;
                case OpCode.Pop:
                    VAL = Pop();
                    continue;
                case OpCode.Lit:
                    VAL = Template.Slots[IR & 0x00FFFFFFFFFFFFFF];
                    continue;
                case OpCode.PushContinuation:
                    /* When a procedure performs a non-tail procedure call,
                     it packages its important state information up into a partial continuation;
                     this record saves the values of
                     1) the environment,
                     2) template,
                     3) PC,and
                     4) continuation registers, and
                     5) any temporary values on the eval stack. */
                    CONT = new PartialContinuation(
                        Template,
                        IR & 0x00FFFFFFFFFFFFFF,
                        ENVT,
                        EvalStack,
                        CONT);
                    EvalStack = Jig.List.Null;
                    continue;
                case OpCode.PopContinuation:
                    if (CONT is PartialContinuation pc) {
                        PC = pc.ReturnAddress;
                        EvalStack = pc.EvalStack;
                        ENVT = pc.Environment;
                        Template = pc.Template;
                        CONT = pc.Continuation;
                        continue;
                    }
                    return VAL;
                case OpCode.Call:
                    if (VAL is Procedure proc) {
                        ENVT = proc.Environment; // NOTE: procedure is responsible for extending environment
                        Template = proc.Template;
                        PC = 0ul;
                        continue;
                    }

                    if (VAL is PartialContinuation cont) {
                        CONT = cont.Continuation;
                        ENVT = cont.Environment;
                        Template = cont.Template;
                        PC = cont.ReturnAddress;
                        if (EvalStack is Jig.List.NonEmpty properList) {
                            if (properList.Count() != 1)
                                throw new Exception("continuations that take multiple values not currently supported");
                            VAL = (Form)properList.First;
                        }

                        continue;
                    }

                    throw new Exception($"VM: in Call @ {PC}: expected procedure or continuation, got {VAL.Print()}");
                case OpCode.Top:
                    VAL = Template.Bindings[IR & 0x00FFFFFFFFFFFFFF].Slot;
                    continue;
                case OpCode.LexVar:
                    var v = Template.Bindings[IR & 0x00FFFFFFFFFFFFFF].Slot;
                    VAL = v ?? throw new Exception($"tried to dereference undeclared variable");
                    continue;
                case OpCode.SetLex:
                    if (Template.Bindings[IR & 0x00FFFFFFFFFFFFFF].Slot is null)
                        throw new Exception($"tried to set undeclared lexical variable");
                    Template.Bindings[IR & 0x00FFFFFFFFFFFFFF].Slot = Pop();
                    VAL = Form.Void;
                    continue;
                case OpCode.Bind:
                    ulong parameterIndex = IR & 0x00FFFFFFFFFFFFFF;
                    Template.Bindings[parameterIndex].Slot = Pop();
                    continue;
                case OpCode.BindRest:
                    ulong restIndex = IR & 0x00FFFFFFFFFFFFFF;
                    Template.Bindings[restIndex].Slot = EvalStack;
                    ClearStack();
                    continue;
                case OpCode.Def:
                    ulong slot = IR & 0x00FFFFFFFFFFFFFF;
                    Template.Bindings[slot].Slot = VAL;
                    VAL = Form.Void;
                    continue;
                case OpCode.SetTop:
                    ulong symIndex = IR & 0x00FFFFFFFFFFFFFF;
                    Template.Bindings[symIndex].Slot = VAL;
                    VAL = Form.Void;
                    continue;
                case OpCode.Jump:
                    PC = IR & 0x00FFFFFFFFFFFFFF;
                    continue;
                case OpCode.JumpIfFalse:
                    if (Bool.False.Equals(VAL)) {
                        PC = IR & 0x00FFFFFFFFFFFFFF;
                    }

                    continue;
                case OpCode.Closure:
                    var t = (Template)Pop();
                    var e = (VM.Environment)Pop();
                    VAL = new Procedure(e, t);
                    continue;
                case OpCode.Env:
                    VAL = ENVT;
                    continue;
                case OpCode.Cons:
                    // TODO: validate EvalStack
                    var car = Pop();
                    var cdr = Pop();
                    VAL = (Jig.Form)Pair.Cons(car, cdr);
                    continue;
                case OpCode.Car:
                    var cons = Pop();
                    if (cons is IPair pair) {
                        VAL = (Jig.Form)pair.Car;
                        continue;
                    }

                    throw new Exception("car: expected pair argument");
                case OpCode.Cdr:
                    var cns = Pop();
                    if (cns is IPair p) {
                        VAL = (Jig.Form)p.Cdr;
                        continue;
                    }

                    throw new Exception("cdr: expected pair argument");
                case OpCode.NullP:
                    var arg = Pop();
                    if (arg is IEmptyList) {
                        VAL = Jig.Bool.True;
                        continue;
                    }

                    VAL = Jig.Bool.False;
                    continue;
                case OpCode.Sum:
                    VAL = Integer.Zero;
                    while (EvalStack is not IEmptyList) {
                        VAL = (Number)VAL + (Number)Pop();
                    }

                    continue;
                case OpCode.ZeroP:
                    // TODO: check arg type
                    if (Integer.Zero.Equals(Pop())) {
                        VAL = Bool.True;
                        continue;
                    }

                    VAL = Bool.False;
                    continue;
                default: throw new Exception($"unhandled case {opCode} in Execute");
            }
        }

    }

    public void Load(Template program, Environment env) {
        PC = 0;
        Template = program;
        ClearStack();
        ENVT = env;
        CONT = new TopLevelContinuation(env);
    }


    // public Form Run() {
    //     while (PC != ulong.MaxValue) {
    //         Fetch();
    //         Execute();
    //     }
    //
    //     return VAL;
    // }
}