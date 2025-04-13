using System.Diagnostics;
using Jig;
namespace VM;

public delegate void ContinuationAny(params Form[] forms);

public class Machine {

    public Machine(uint stacksize = 512) {
        StackSize = stacksize;
        Stack = new Form[stacksize];
    }

    internal ulong IR;
    
    internal ulong PC;


    public Form VAL = Form.Void;

    public void DoResults(Action<Form> action) {
        while (SP > FP) {
            action(Pop());
        }

    }


    internal Template Template;

    internal Form[] Stack;
    
    private uint StackSize;

    internal uint SP = 0;
    internal uint FP = 0;

    internal Continuation CONT;

    internal Environment ENVT;

    internal Form Pop() {
        if (SP == 0) throw new Exception("stack is empty");
        SP--;
        Form result = Stack[SP];
        // Console.WriteLine($"popped {result.Print()}. stack = {StackToList().Print()}");
        return result;
    }

    internal void Push(Form val) {
        if (SP == StackSize) {
            throw new Exception("stack is full");
        }
        Stack[SP] = val;
        SP++;
        // Console.WriteLine($"pushed {val.Print()}. stack = {StackToList().Print()}");
    }

    private void ClearStack() {
        SP = 0;
        FP = 0;
    }

    private void Call() {
        
        if (VAL is Procedure proc) {
            if (proc.HasRest) {
                if (SP - FP < proc.Required) {
                    throw new Exception(
                        $"wrong num args: expected at least {proc.Required}, but got only {SP - FP}");
                }
            } else {
                if (SP - FP != proc.Required) {
                    
                    Array.ForEach(Dissassembler.Disassemble(proc.Template), Console.WriteLine);
                    throw new Exception($"wrong num args: expected {proc.Required}, but got {SP - FP}. (SP = {SP}; FP = {FP}; stack = {StackToList()})");
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
        throw new Exception($"VM: in Call @ {PC}: expected procedure or continuation, got {VAL.Print()}");
    }

    Form[] SaveStackFrameToArray() {
        var results = new Form[SP - FP];
        Array.Copy(Stack, FP, results, 0, results.Length);
        return results;
    }

    internal Jig.List StackToList() {
        Jig.List result = List.Null;
        for (int i = 0; i < SP; i++) {
            result = Jig.List.Cons(Stack[i], result);
        }
        return result;
    }

    private Jig.List ConsumeStackFrameToList() {
        Jig.List result = List.Null;
        for (uint i = FP; i < SP; i++) {
            result = Jig.List.Cons(Stack[i], result);
        }

        SP = FP;
        // Console.WriteLine($"popped {result.Print()}. stack = {StackToList().Print()}");
        return result;
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
                case OpCode.PushContinuationForArg:
                    /* When a procedure performs a non-tail procedure call,
                     it packages its important state information up into a partial continuation;
                     this record saves the values of
                     1) the environment,
                     2) template,
                     3) PC,and
                     4) continuation registers, and
                     5) any temporary values on the eval stack. */
                    CONT = new ContinuationForArg(
                        Template,
                        IR & 0x00FFFFFFFFFFFFFF,
                        ENVT,
                        FP,
                        CONT);
                    FP = SP;
                    continue;
                case OpCode.PushContinuationForNonTailBody:
                    CONT = new ContinuationForNonTailBody(
                        Template,
                        IR & 0x00FFFFFFFFFFFFFF,
                        ENVT,
                        FP,
                        CONT);
                    FP = SP;
                    continue;
                case OpCode.PopContinuation:
                    if (CONT is PartialContinuation pc) {
                        pc.Pop(this);
                        continue;
                    }
                    CONT.Pop(this);
                    return;
                case OpCode.Call:
                    // TODO: VAL already has the procedure
                    // seems silly to pop it into VAL again
                    // but then we have to NOT push it onto stack
                    
                    // maybe by having another CompilationContext for operator
                    VAL = Pop();
                    if (VAL is Primitive primitiveFn) {
                        // primitives don't have a return instruction, thus this ugliness:
                        Delegate del = primitiveFn.Delegate;
                        // TODO:
                        VAL = (Form)del.DynamicInvoke(ConsumeStackFrameToList().Cast<object>().ToArray());
                        Push(VAL);
                        if (CONT is PartialContinuation pct) {
                            pct.Pop(this);
                            continue;
                        }
                        CONT.Pop(this);
                        return;
                    }
                    if (VAL is SavedContinuation cont) {
                        var results = SaveStackFrameToArray();
                        cont.Apply(this, results);
                        if (cont.Saved is TopLevelContinuation) {
                            return;
                        }
                        continue;
                    }
                    Call();
                    continue;
                case OpCode.CallWValues:
                    var producer = (Procedure)Pop();
                    var continuationProc = (Procedure)Pop();
                    CONT = new PartialContinuationForCallWithValues(
                        continuationProc.Template,
                        0,
                        continuationProc.Environment,
                        FP,
                        CONT,
                        continuationProc.Required,
                        continuationProc.HasRest);
                    VAL = producer;
                    Call();
                    continue;
                case OpCode.CallCC:
                    VAL = (Procedure)Pop(); // put the lambda expr or procedure in VAL
                    Push(new SavedContinuation(CONT, Stack.Take((int)SP).ToArray()));
                    // the following line is wrong because even though
                    // the call instruction expects procedure to be on the stack, the 
                    // Call function does not. Ew.
                    // Push(VAL);
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
                    // TODO:
                    var xs = new System.Collections.Generic.List<Form>();
                    while (SP != FP) {
                        xs.Add(Pop());
                    }
                    ENVT.BindParameter(restIndex, xs.ToJigList());
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
                    VAL = ENVT.SetLocal(h, x, Pop());
                    continue;
                case OpCode.Store:
                    Template.Bindings[IR & 0x00FFFFFFFFFFFFFF].Slot = Pop();
                    continue;
                case OpCode.DefLocal:
                    int i = (int)(IR & 0x00000000FFFFFFFF);
                    ENVT.DefLocal(i, Pop());
                    continue;
                case OpCode.Jump:
                    PC = IR & 0x00FFFFFFFFFFFFFF;
                    continue;
                case OpCode.JumpIfFalse:
                    if (Bool.False.Equals(Pop())) {
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
                    while (FP < SP) {
                        VAL = (Number)VAL + (Number)Pop();
                    }
                    Push(VAL);
                    continue;
                case OpCode.Product:
                    VAL = Integer.One;
                    while (FP < SP) {
                        Number p1 = (Number)Pop();
                        VAL = (Number)VAL * p1 ;
                    }
                    Push(VAL);
                    continue;
                default: throw new Exception($"unhandled case {opCode} in Execute");
            }
        }

    }

    public void Load(Template program, Environment env, ContinuationAny cont) {
        PC = 0;
        Template = program;
        ClearStack();
        ENVT = env;
        CONT = new TopLevelContinuation(cont);
    }


}
