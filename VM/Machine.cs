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

    
    internal void Fetch() {
        Debug.Assert(PC < (ulong)Template.Code.Length, $"PC was {PC} and Template length was {Template.Code.Length}");
        IR = Template.Code[PC];
        PC++;
    }

    internal void Execute() {
        OpCode opCode = (OpCode)(IR >> 56);
        switch (opCode) {
            case OpCode.Push:
                Push(VAL);
                return;
            case OpCode.Pop:
                VAL = Pop();
                return;
            case OpCode.Lit:
                VAL = Template.Slots[IR & 0x00FFFFFFFFFFFFFF];
                return;
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
                    CONT.Environment,
                    EvalStack,
                    CONT);
                EvalStack = Jig.List.Null;
                return;
            case OpCode.PopContinuation:
                if (CONT is PartialContinuation partialContinuation) {
                    CONT = partialContinuation.Continuation;
                    PC = CONT.ReturnAddress;
                    EvalStack = CONT.EvalStack;
                    ENVT = CONT.Environment;
                    return;
                }

                PC = CONT.ReturnAddress;
                return;
            case OpCode.Call:
                if (VAL is Procedure proc) {
                    ENVT = proc.Environment; // NOTE: procedure is responsible for extending environment
                    Template = proc.Template;
                    PC = 0ul;
                    return;
                }
                if (VAL is PartialContinuation cont) {
                    CONT = cont.Continuation;
                    ENVT = cont.Environment;
                    Template = cont.Template;
                    PC = cont.ReturnAddress;
                    if (EvalStack is Jig.List.NonEmpty properList) {
                        if (properList.Count() != 1) throw new Exception("continuations that take multiple values not currently supported");
                        VAL = (Form)properList.First;
                    }
                    return;
                }
                throw new Exception($"expected procedure or continuation, got {VAL.Print()}");
            case OpCode.Top:
                VAL = Template.Globals[IR & 0x00FFFFFFFFFFFFFF].Slot;
                return;
            case OpCode.LexVar:
                var v = ENVT.LexVar[IR & 0x00FFFFFFFFFFFFFF];
                VAL = v ?? throw new Exception($"tried to dereference undeclared variable");
                return;
            case OpCode.SetLex:
                if (ENVT.LexVar[IR & 0x00FFFFFFFFFFFFFF] is null)
                    throw new Exception($"tried to set undeclared lexical variable");
                ENVT.LexVar[IR & 0x00FFFFFFFFFFFFFF] = Pop();
                VAL = Form.Void;
                return;
            case OpCode.Bind:
                ulong parameterIndex = IR & 0x00FFFFFFFFFFFFFF;
                ENVT.LexVar[parameterIndex] = Pop();
                return;
            case OpCode.BindRest:
                ulong restIndex = IR & 0x00FFFFFFFFFFFFFF;
                ENVT.LexVar[restIndex] = EvalStack;
                ClearStack();
                return;
            case OpCode.DefLocal:
                ulong slot = IR & 0x00FFFFFFFFFFFFFF;
                ENVT.LexVar[slot] = Pop();
                VAL = Form.Void;
                return;
            case OpCode.DefTop:
                ulong symbolIndex = IR & 0x00FFFFFFFFFFFFFF;
                Template.Globals[symbolIndex].Slot = Pop();
                VAL = Form.Void;
                return;
            case OpCode.SetTop:
                ulong symIndex = IR & 0x00FFFFFFFFFFFFFF;
                Template.Globals[symIndex].Slot = Pop();
                VAL = Form.Void;
                return;
            case OpCode.Jump:
                PC = IR & 0x00FFFFFFFFFFFFFF;
                return;
            case OpCode.JumpIfFalse:
                if (Bool.False.Equals(VAL)) {
                    PC = IR & 0x00FFFFFFFFFFFFFF;
                }
                return;
            case OpCode.Closure:
                var t = (Template)Pop();
                var e = (VM.Environment)Pop();
                VAL = new Procedure(e, t);
                return;
            case OpCode.Env:
                VAL = ENVT;
                return;
            default: throw new Exception($"unhandled case {opCode} in Execute");
        }

    }

    public void Load(Template program, Environment env) {
        PC = 0;
        Template = program;
        ClearStack();
        ENVT = env;
        CONT = new TopLevelContinuation(env);





    }


    public Form Run() {
        while (PC != ulong.MaxValue) {
            Fetch();
            Execute();
        }

        return VAL;
    }
}