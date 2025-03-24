using System.Diagnostics;
using Jig;
namespace VM;

public class Machine {

    internal ulong IR;
    
    internal ulong PC;


    public Form VAL {
        get {
            RC = 0;
            return R[0];
        }
        internal set {
            R[0] = value;
            RC = 1;
        }
    }

    internal const int RN = 8;
    
    public Form[] R = new Form[RN];

    public int RC = 0;
    
    public Queue<Form> RR = new();

    public void DoResults(Action<Form> action) {
        for (int i = 0; i < RC; ++i) {
            if (i < RN) {
                if (R[i] is not Form.VoidType) {
                    action(R[i]);
                }
            } else {
                var f = RR.Dequeue();
                if (f is not Form.VoidType) {
                    action(f);
                }
            }
        }

        RC = 0;

    }


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

    private void Call() {
        
        if (VAL is Procedure proc) {
            if (proc.HasRest) {
                if (EvalStack.Count() < proc.Required) {
                    throw new Exception(
                        $"wrong num args: expected at least {proc.Required}, but got only {EvalStack.Count()}");
                }
            } else {
                if (EvalStack.Count() != proc.Required) {
                    throw new Exception($"wrong num args: expected {proc.Required}, but got {EvalStack.Count()}");
                }  
            }
            if (proc.Environment is not null) {
                // TODO: why is it possible for Environment to be null?
                ENVT = proc.Environment.Extend(proc.Template.NumVarsForScope);
            }
            Template = proc.Template;
            PC = 0ul;
            return;
        }
        if (VAL is PartialContinuation cont) {
            CONT = cont.Continuation;
            Template = cont.Template;
            PC = cont.ReturnAddress;
            ENVT = cont.Environment;
            if (EvalStack is Jig.List.NonEmpty properList) {
                if (properList.Count() != 1)
                    throw new Exception("continuations that take multiple values not currently supported");
                VAL = (Form)properList.First;
            }
            EvalStack = cont.EvalStack;
            return;
        }
        throw new Exception($"VM: in Call @ {PC}: expected procedure or continuation, got {VAL.Print()}");
    }

    

    public void Run() {
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
                    // TODO: we need different kinds of continuations. so prob new opcodes or args to opcodes
                    CONT = new PartialContinuation(
                        Template,
                        IR & 0x00FFFFFFFFFFFFFF,
                        ENVT,
                        EvalStack,
                        CONT,
                        1,
                        false);
                    EvalStack = Jig.List.Null;
                    continue;
                case OpCode.PopContinuation:
                    if (CONT is PartialContinuation pc) {
                        // check that continuation we are returning to expects the number of values
                        if (pc.Continuation.HasOptional) {
                            if (RC < pc.Continuation.Required) {
                                throw new Exception(
                                    $"continuation expected at least {pc.Continuation.Required} values, but received {RC}");
                            }
                        } else {
                            if (RC != pc.Continuation.Required) {
                                throw new Exception(
                                    $"continuation expected {pc.Continuation.Required} values, but received {RC}");
                            
                            }
                        }
                        PC = pc.ReturnAddress;
                        if (pc is PartialContinuationForCallWithValues) {
                            Debug.Assert(EvalStack is IEmptyList);
                            int n = RC - 1;
                            if (n >= RN) {
                                Stack<Form> stack = new Stack<Form>(RR);
                                for (; n >= RN; n--) {
                                    var fromQ = stack.Pop();
                                    Push(fromQ);
                                }
                            } 
                            for (; n >= 0; n--) {
                                Push(R[n]);
                            }
                            RC = 0;
                        } else {
                            EvalStack = pc.EvalStack;
                        }

                        if (pc is PartialContinuationForCallWithValues) {
                            if (pc.Environment is not null) {
                                // TODO: again why?
                                // TODO: we are reproducing code from Call() :(
                                ENVT = pc.Environment.Extend(pc.Template.NumVarsForScope);
                            }
                        } else {
                            ENVT = pc.Environment;
                        }
                        Template = pc.Template;
                        CONT = pc.Continuation;
                        // TODO: how does RC get set back to zero?
                        // do only values and call with values need to use RC?
                        continue;
                    }
                    return;
                case OpCode.Call:
                    if (VAL is Primitive primitiveFn) {
                        // primitives don't have a return instruction, thus this ugliness:
                        Delegate del = primitiveFn.Delegate;
                        VAL = (Form)del.DynamicInvoke(EvalStack.Cast<object>().ToArray());
                        if (CONT is PartialContinuation pct) {
                            PC = pct.ReturnAddress;
                            EvalStack = pct.EvalStack;
                            ENVT = pct.Environment;
                            Template = pct.Template;
                            CONT = pct.Continuation;
                            continue;
                        }
                        return;
                    }
                    Call();
                    continue;
                case OpCode.Values:
                    while (EvalStack is not IEmptyList) {
                        if (RC < RN) {
                            R[RC++] = Pop();

                        } else {
                            RR.Enqueue(Pop());
                            RC++;
                        }
                    }
                    continue;
                case OpCode.CallWValues:
                    var producer = (Procedure)Pop();
                    var continuationProc = (Procedure)Pop();
                    CONT = new PartialContinuationForCallWithValues(
                        continuationProc.Template,
                        0,
                        continuationProc.Environment,
                        EvalStack,
                        CONT,
                        continuationProc.Required,
                        continuationProc.HasRest);
                    VAL = producer;
                    Call();
                    continue;
                case OpCode.CallCC:
                    VAL = (Procedure)Pop();
                    Push(CONT);
                    Call();
                    continue;
                case OpCode.Bind:
                    int parameterNumber = (int)(IR & 0x00000000FFFFFFFF);
                    for (int n = 0; n < parameterNumber; n++) {
                        ENVT.BindParameter(n, Pop());
                    }
                    continue;
                case OpCode.BindRest:
                    // bind zero should always be called first
                    int restIndex = (int)(IR & 0x00FFFFFFFFFFFFFF);
                    ENVT.BindParameter(restIndex, EvalStack);
                    ClearStack();
                    continue;
                case OpCode.Load:
                    VAL = Template.Bindings[IR & 0x00FFFFFFFFFFFFFF].Slot;
                    continue;
                case OpCode.Local:
                    int index = (int)(IR & 0x00000000FFFFFFFF);
                    int depth = (int)(IR >> 32) & 0x00FFFFFF ;
                    VAL = ENVT.GetLocal(depth, index);
                    continue;
                case OpCode.SetLex:
                    int x = (int)(IR & 0x00000000FFFFFFFF);
                    int h = (int)(IR >> 32) & 0x00FFFFFF ;
                    VAL = ENVT.SetLocal(h, x, VAL);
                    continue;
                case OpCode.Store:
                    Template.Bindings[IR & 0x00FFFFFFFFFFFFFF].Slot = VAL;
                    VAL = Form.Void;
                    continue;
                case OpCode.DefLocal:
                    int i = (int)(IR & 0x00000000FFFFFFFF);
                    ENVT.DefLocal(i, VAL);
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
                case OpCode.Lambda:
                    var t = (Template)Pop();
                    var e = (VM.Environment)Pop();
                    VAL = new Procedure(e, t);
                    continue;
                case OpCode.Env:
                    VAL = ENVT;
                    continue;
                case OpCode.Sum:
                    VAL = Integer.Zero;
                    while (EvalStack is not IEmptyList) {
                        VAL = (Number)VAL + (Number)Pop();
                    }
                    continue;
                case OpCode.Product:
                    // Console.WriteLine($"* applied with args {EvalStack.Print()}");
                    VAL = Integer.One;
                    while (EvalStack is not IEmptyList) {
                        Number p1 = (Number)Pop();
                        // Console.WriteLine($"multiplying {VAL} and {p1} ");
                        VAL = (Number)VAL * p1 ;
                    }
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
