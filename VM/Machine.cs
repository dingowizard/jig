using System.Diagnostics;
using System.Security.Cryptography;
using Jig;
namespace VM;

public delegate void ContinuationAny(params Form[] forms);

public class Machine
{

    internal bool Loud = false;

    public Machine(uint stackSize = 512) {
        StackSize = stackSize;
        Stack = new Form[stackSize];
    }

    internal Winders Winders = new Winders();
    

    private ulong IR;
    
    internal ulong PC;


    public Form VAL = Form.Void;

    internal Template Template;

    internal readonly Form[] Stack;
    
    private readonly uint StackSize;

    internal uint SP = 0;
    internal uint FP = 0;

    internal Continuation CONT;

    internal Environment ENVT;

    internal Form Pop() {
        if (SP == 0) throw new Exception("stack is empty");
        SP--;
        var result = Stack[SP];
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

    internal void Call() {
        
        if (VAL is Procedure proc) {
            if (proc.HasRest) {
                if (SP - FP < proc.Required) {
                    throw new Exception(
                        $"wrong num args: expected at least {proc.Required}, but got only {SP - FP}");
                }
            } else {
                if (SP - FP != proc.Required) {
                    
                    Array.ForEach(Disassembler.Disassemble(proc.Template), Console.WriteLine);
                    throw new Exception($"wrong num args: expected {proc.Required}, but got {SP - FP}. (SP = {SP}; FP = {FP}; stack = {StackToList()})");
                }  
            }
            ENVT = proc.Environment.Extend(proc.Template.NumVarsForScope);
            Template = proc.Template;
            PC = 0ul;
            return;
        }

        if (VAL is SavedContinuation sc) {
            sc.Apply(this);
            return;

        }
        throw new Exception($"VM: in Call @ {PC - 1}: expected procedure or saved continuation, got {VAL.GetType()}");
    }

    internal Form[] SaveStackFrameToArray() {
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
    

    public void Run()
    {
        Loud = false;
        while (true) {
            // Fetch
            // Debug.Assert(PC < (ulong)Template.Code.Length,
            //     $"PC was {PC} and Template length was {Template.Code.Length}");
            if (PC >= (ulong)Template.Code.Length)
            {
                
                Console.WriteLine($"*** PC ({PC}) ran past template ***");
                Array.ForEach(Disassembler.Disassemble(Template), Console.WriteLine);
                throw new Exception($"");
            }
            IR = Template.Code[PC];
            PC++;
            // Execute
            OpCode opCode = (OpCode)(IR >> 56);
            if (Loud) Console.WriteLine(Disassembler.Decode((int)PC - 1, IR, Template.Slots, Template.Bindings));
            switch (opCode) { 
                case OpCode.Push:
                    Push(VAL!);
                    continue;
                case OpCode.Pop:
                    VAL = Pop();
                    continue;
                case OpCode.PushFP:
                    Push(new Integer((int)FP)); // TODO: FP can be too big?
                    continue;
                case OpCode.PopFP:
                    FP = (uint)((Integer)Pop()).Value;
                    continue;
                case OpCode.SPToFP:
                    FP = SP;
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
                case OpCode.PushContinuationForBodyThunk:
                    // NOTE: unlike other PushContinuations,
                    // we're not doing anything with the FP,
                    // the surrounding code is responsible
                    CONT = new PartialContinuation(
                        Template,
                        IR & 0x00FFFFFFFFFFFFFF,
                        ENVT,
                        FP,
                        CONT,
                        0,
                        true);
                    continue;
                case OpCode.PopContinuation:
                    // Console.WriteLine(Dissassembler.Decode((int)PC, IR, Template.Slots, Template.Bindings));
                    if (CONT is TopLevelContinuation)
                    {
                        CONT.Pop(this);
                        // Console.WriteLine($"about to return from case OpCode.PopContinuation");
                        return;
                        
                    }

                    if (CONT is WinderThunkCont.ThunkCont wtc)
                    {
                        // Console.WriteLine($"VM.PopContinuation: PC = {PC} wtc is {wtc.GetType()}");

                        if (wtc.Continuation is WinderThunkCont.BaseCont bc &&
                            bc.SavedContinuation.Saved is TopLevelContinuation)
                        {
                            wtc.Pop(this);
                            return;

                        }
                        
                    }

                    CONT.Pop(this);
                    continue;
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
                        // Console.WriteLine($"about to return from case OpCode.Call, PrimitiveFn");
                        return;
                    }
                    if (VAL is SavedContinuation cont) {
                        // Console.WriteLine($"applying a saved continuation");
                        // Console.WriteLine($"\tcont.SavedWinders.Length = {cont.SavedWinders.Length} and Winders = {Winders.Length}");
                        // Console.WriteLine($"\tthey are ReferenceEquals: {object.ReferenceEquals(cont.SavedWinders, Winders) }");
                        if (!cont.SavedWinders.Equals(Winders)) { // TODO: ReferenceEquals?
                            CONT = cont.SavedWinders.DoWinders(this, cont);
                            if (CONT is not WinderThunkCont.ThunkCont wThunk)
                                throw new Exception(
                                    "if winders aren't equal, we SHOULD have a WinderThunk (or BaseCont?)");
                            // winders need to be called with no arguments, but if we're applying a saved
                            // continuation, there may be arguments
                            // so we should save the frame pointer on the stack
                            // set the fp to sp
                            // then call the first thunk
                            // when we get to the saved continuation itself, after doing the winders, 
                            // we need to pop the fp off the stack
                            Push(new Integer((int)FP));
                            FP = SP;
                            CONT = wThunk.Continuation;
                            VAL = wThunk.Thunk;
                            Call();
                            continue;

                        }
                        cont.Apply(this);
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
                    // Console.WriteLine($"in call-with-values: continuationProc:");
                    // Array.ForEach(Dissassembler.Disassemble(continuationProc.Template), Console.WriteLine);
                    // Console.WriteLine($"required params: {continuationProc.Required}. hasRest? {continuationProc.HasRest}");
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
                    Push(new SavedContinuation(CONT, Stack.Take((int)SP).ToArray(), Winders.Copy()));
                    // the following line is wrong because even though
                    // the call instruction expects procedure to be on the stack, the 
                    // Call function does not. Ew.
                    // Push(VAL);
                    Call();
                    continue;
                case OpCode.PushWinder:
                    Procedure beforeThunk = (Procedure)Pop();
                    Procedure afterThunk = (Procedure)Pop();
                    this.Winders.Push(beforeThunk, afterThunk);
                    continue;
                case OpCode.PopWinder:
                    this.Winders.Pop();
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
                case OpCode.Top:
                    VAL = Template.Bindings[IR & 0x00FFFFFFFFFFFFFF].Slot;
                    continue;
                case OpCode.Lex:
                    int index = (int)(IR & 0x00000000FFFFFFFF);
                    int depth = (int)(IR >> 32) & 0x00FFFFFF ;
                    VAL = ENVT.GetLocal(depth, index);
                    continue;
                case OpCode.SetLex:
                    int x = (int)(IR & 0x00000000FFFFFFFF);
                    int h = (int)(IR >> 32) & 0x00FFFFFF ;
                    ENVT.SetLocal(h, x, Pop());
                    continue;
                case OpCode.SetTop:
                    
                    // Console.WriteLine($"VM: executing SetTop instruction");
                    // Array.ForEach(Disassembler.Disassemble(Template), Console.WriteLine);
                    Template.Bindings[IR & 0x00FFFFFFFFFFFFFF].Slot = Pop();
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