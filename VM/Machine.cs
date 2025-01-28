using Jig;
namespace VM;

public class Machine {

    internal ulong IR;
    
    internal ulong PC;

    internal Form VAL = Form.Void;

    internal Template Template;

    internal List EvalStack = Jig.List.Null;

    internal PartialContinuation? CONT;

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

    
    internal void Fetch() {
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
                CONT = CONT.Continuation;
                PC = CONT.ReturnAddress;
                EvalStack = CONT.EvalStack;
                ENVT = CONT.Environment;
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
                VAL = ENVT.LookUpLexVar(
                    (int)(IR & 0x00FFFFFFFF000000) >> 24,
                    (int)(IR & 0x0000000000FFFFFF));
                return;
            case OpCode.SetLex:
                ENVT.SetLexVar(
                    (int)(IR & 0x00FFFFFFFF000000) >> 24,
                    (int)(IR & 0x0000000000FFFFFF),
                    Pop());
                VAL = Form.Void;
                return;
            case OpCode.Bind:
                ulong argNum = IR & 0x00FFFFFFFFFFFFFF;
                if (argNum != (ulong)EvalStack.Count()) {
                    throw new Exception($"expected {argNum} args, but got {EvalStack.Count()}");
                }
                ENVT = ENVT.Extend(EvalStack);
                return;
            case OpCode.BindWRest:
                ulong requiredNo = IR & 0x00FFFFFFFFFFFFFF;
                if (requiredNo < (ulong)EvalStack.Count()) {
                    throw new Exception($"expected at least {requiredNo} args, but got {EvalStack.Count()}");
                }
                ENVT = ENVT.ExtendWRest(EvalStack, requiredNo);
                return;
            case OpCode.DefLocal:
                ulong slot = IR & 0x00FFFFFFFFFFFFFF;
                ENVT.DefineLocal(slot, Pop());
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
            default: throw new Exception($"unhandled case in Execute");
        }

    }
    
    
}