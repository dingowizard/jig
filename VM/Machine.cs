using Jig;
using Jig.Expansion;
namespace VM;


public class Machine : IRuntime {

    private bool Loud = false;

    public Machine(IEvaluator evaluator, Environment env, uint stackSize = 512) {
        _stackSize = stackSize;
        Evaluator = evaluator;
        ENVT = env;
        VARS = [];
        // TODO: why do both Load and this constructor set the CONT?
        CONT = new TopLevelTemplateContinuation(TopLevelContinuation);
        RuntimeEnvironment = ENVT;
        // TODO: no need for dictionary
        CoreSyntax = FreshCoreSyntax().Rules.Select(kvp => (kvp.Key, kvp.Value) ).ToArray();
        Stack = new SchemeValue[stackSize];
        // TODO: replace Template with ulong[] Instructions and Literals
        // NOTE: this will affect the Disassembler and debugging.
        // maybe hold off until we figure out debug table and stack tracing
        Template = Template.Empty;
    }
    
    internal Winders Winders = new Winders();
    

    private ulong IR;
    
    internal ulong PC;


    public SchemeValue VAL = SchemeValue.Void;

    public Location[] VARS = [];

    internal Template Template;

    internal readonly SchemeValue[] Stack;
    
    private readonly uint _stackSize;

    internal uint SP = 0;
    internal uint FP = 0;

    internal Continuation CONT;

    internal Environment ENVT;

    public IEvaluator Evaluator;

    internal SchemeValue Pop() {
        if (SP == 0) throw new Exception("stack is empty");
        SP--;
        var result = Stack[SP];
        // Console.WriteLine($"popped {result.Print()}. stack = {StackToList().Print()}");
        return result;
    }

    internal void Push(SchemeValue val) {
        if (SP == _stackSize) {
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
        
        // Console.WriteLine($"call: (stack is {StackToList().Print()}");
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
            // NOTE: the reason why the procedure code has the job of binding the args
            // is because of how primitives work. If the runtime bound the args for the procedures,
            // then there would be nothing on the stack for the primitive functions to use.
            // probably Primitives could be redesigned though.
            // For example, primitives could get the arg values from the environment rather than the stack.
            System.Collections.Generic.List<SchemeValue> args = [];
            while (SP > FP) {
                args.Add(Pop());
            }
            ENVT = proc.Environment.ExtendForProcCall(proc, args);
            VARS = proc.Locations;
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

    internal SchemeValue[] SaveStackFrameToArray() {
        var results = new SchemeValue[SP - FP];
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
        // Array.ForEach(Disassembler.Disassemble(Template), Console.WriteLine);
        // Console.WriteLine("VARS:");
        // int locNo = 0;
        // foreach (var loc in VARS)
        // {
        //     Console.WriteLine($"\t{locNo++}\t{loc.GetHashCode()}");
        //
        // }
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
            if (Loud) Console.WriteLine(Disassembler.Decode((int)PC - 1, IR, Template.Literals, Template.Vars));
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
                    VAL = Template.Literals[IR & 0x00FFFFFFFFFFFFFF];
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
                        VARS,
                        FP,
                        CONT);
                    FP = SP;
                    continue;
                case OpCode.PushContinuationForNonTailBody:
                    CONT = new ContinuationForNonTailBody(
                        Template,
                        IR & 0x00FFFFFFFFFFFFFF,
                        ENVT,
                        VARS,
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
                        VARS,
                        FP,
                        CONT,
                        0,
                        true);
                    continue;
                case OpCode.PopContinuation:
                    CONT.Pop(this);
                    continue;
                case OpCode.Call:
                    // TODO: VAL already has the procedure
                    // seems silly to pop it into VAL again
                    // but then we have to NOT push it onto stack
                    
                    // maybe by having another CompilationContext for operator
                    VAL = Pop();
                    if (VAL is Primitive primitiveProc) {
                        primitiveProc.Apply(this);
                        // TODO: does CONT.Pop belong in the Primitive procedure?
                        CONT.Pop(this);
                        continue;
                    }
                    
                    if (VAL is SavedContinuation cont) {
                        // When you apply a saved continuation, you need to do the winders first.
                        // We'll make a continuation that does all the winders. It's continuation will be this one.
                        // That will get applied below. After the winders are done
                        // We'll apply the saved continuation
                        // TODO: could all this logic be inside SavedContinuation.Apply?
                        // maybe not. It's making my brain hurt. 
                        if (!cont.SavedWinders.Equals(Winders)) { // TODO: ReferenceEquals?
                            CONT = cont.SavedWinders.DoWinders(this, cont);
                            if (CONT is not WinderThunkCont.ThunkCont wThunk)
                                throw new Exception( "if winders aren't equal, we SHOULD have a WinderThunk (or BaseCont?)");
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
                            Call(); // When we Call, the procedure is wThunk.Thunk, stored in VAL ... so NOT a SavedContinuation
                            continue;

                        }
                        cont.Apply(this);
                        // if (cont.Saved is TopLevelContinuation) {
                        //     return;
                        // }
                        continue;
                    }
                    Call();
                    continue;
                case OpCode.CallWValues:
                    var producer = (Procedure)ENVT.GetArg(0);
                    var continuationProc = (Procedure)ENVT.GetArg(1);
                    // Console.WriteLine($"in call-with-values: continuationProc:");
                    // Array.ForEach(Dissassembler.Disassemble(continuationProc.Template), Console.WriteLine);
                    // Console.WriteLine($"required params: {continuationProc.Required}. hasRest? {continuationProc.HasRest}");
                    CONT = new PartialContinuationForCallWithValues(
                        continuationProc.Template,
                        0,
                        continuationProc.Environment,
                        continuationProc.Locations, // TODO: consistent naming
                        FP,
                        CONT,
                        continuationProc.Required,
                        continuationProc.HasRest);
                    VAL = producer;
                    Call();
                    continue;
                case OpCode.CallCC:
                    VAL = (Procedure)ENVT.GetArg(0); // put the lambda expr or procedure in VAL
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
                // case OpCode.Bind:
                //     // NOTE: the reason why the procedure code has the job of binding the args
                //     // is because of how primitives work. If the runtime bound the args for the procedures,
                //     // then there would be nothing on the stack for the primitive functions to use.
                //     ulong parameterNumber = (IR & 0x00000000FFFFFFFF);
                //     for (ulong n = 0; n < parameterNumber; n++) {
                //         try
                //         {
                //             ENVT.BindParameter(n, Pop());
                //         }
                //         catch (Exception) {
                //             Console.WriteLine($"VM @ {PC - 1}: trying to bind parameter in template:");
                //             Array.ForEach(Disassembler.Disassemble(Template), Console.WriteLine);
                //             Console.WriteLine($"Stack is {StackToList()}");
                //             
                //         }
                //     }
                //     continue;
                // case OpCode.BindRest:
                //     // bind zero should always be called first
                //     ulong restIndex = (IR & 0x00FFFFFFFFFFFFFF);
                //     // TODO:
                //     var xs = new System.Collections.Generic.List<SchemeValue>();
                //     while (SP != FP) {
                //         xs.Add(Pop());
                //     }
                //     ENVT.BindParameter(restIndex, xs.ToJigList());
                //     continue;
                case OpCode.Arg:
                    VAL = ENVT.GetArg(IR & 0x00FFFFFFFFFFFFFF);
                    continue;
                case OpCode.Top:
                    // TODO: currently, referencing a undefined top level var
                    // is a syntax error.
                    // but it should be a runtime error.
                    try
                    {
                        VAL = VARS[IR & 0x00FFFFFFFFFFFFFF].Value;
                    }
                    catch (Exception)
                    {
                        Console.WriteLine($"PC = {PC}");
                        Console.WriteLine($"VARS count = {VARS.Length}. index was {IR & 0x00FFFFFFFFFFFFFF}");
                        Array.ForEach(Disassembler.Disassemble(Template), Console.WriteLine);
                        throw;
                    }
                    continue;
                case OpCode.Lex:
                    // TODO: no need for two opcodes if code is the same?
                    // int index = (int)(IR & 0x00000000FFFFFFFF);
                    // int depth = (int)(IR >> 32) & 0x00FFFFFF ;
                    try {
                        VAL = VARS[IR & 0x00FFFFFFFFFFFFFF].Value;
                    }
                    catch (Exception) {
                        Console.WriteLine($"PC = {PC - 1}");
                        if (VARS is null) Console.WriteLine("VARS is null");
                        if (VARS[IR & 0x00FFFFFFFFFFFFFF] is null) Console.WriteLine($"VARS[{IR & 0x00FFFFFFFFFFFFFF}] is null");
                        Console.WriteLine($"VARS count = {VARS.Length}. index was {IR & 0x00FFFFFFFFFFFFFF}");
                        Array.ForEach(Disassembler.Disassemble(Template), Console.WriteLine);
                        throw;
                    }
                    continue;
                case OpCode.SetArg:
                    try
                    {
                        ENVT.SetArg(IR & 0x00FFFFFFFFFFFFFF, VAL = Pop());
                    }
                    catch (Exception exc)
                    {
                        Console.WriteLine($"PC = {PC}");
                        // Console.WriteLine($"ARGS count = {ENVT.ArgsLength}. index was {IR & 0x00FFFFFFFFFFFFFF}");
                        Array.ForEach(Disassembler.Disassemble(Template), Console.WriteLine);
                        throw;
                    }
                    continue;
                case OpCode.DefVar:
                    // TODO: When is this used? for top levels?
                    // we should simplify the number of instructions for putting a value in a location
                    // Also for when we make a location
                    try {
                        VARS[IR & 0x00FFFFFFFFFFFFFF] = new Location(Pop());
                        VAL = SchemeValue.Void;
                        
                    }
                    catch (Exception) {
                        Console.WriteLine($"PC = {PC}");
                        // Console.WriteLine($"ARGS count = {ENVT.ArgsLength}. index was {IR & 0x00FFFFFFFFFFFFFF}");
                        Array.ForEach(Disassembler.Disassemble(Template), Console.WriteLine);
                        throw;
                    }
                    continue;
                
                case OpCode.DefArg:
                    // An arg is either an argument or a variable defined at
                    // the top of a lambda body.
                    try {
                        ENVT.DefArg(IR & 0x00FFFFFFFFFFFFFF, VAL = Pop());
                    }
                    catch (Exception exc)
                    {
                        Console.WriteLine($"PC = {PC - 1}");
                        // Console.WriteLine($"ARGS count = {ENVT.ArgsLength}. index was {IR & 0x00FFFFFFFFFFFFFF}");
                        Array.ForEach(Disassembler.Disassemble(Template), Console.WriteLine);
                        throw;
                    }
                    continue;
                case OpCode.SetLex:
                    // int x = (int)(IR & 0x00000000FFFFFFFF);
                    // int h = (int)(IR >> 32) & 0x00FFFFFF ;
                    try
                    {
                        VARS[IR & 0x00FFFFFFFFFFFFFF].Value = Pop();
                        // Push(VAL = SchemeValue.Void);
                    }
                    catch (Exception exc)
                    {
                        Console.WriteLine($"PC = {PC}");
                        Console.WriteLine($"VARS count = {VARS.Length}. index was {IR & 0x00FFFFFFFFFFFFFF}");
                        Array.ForEach(Disassembler.Disassemble(Template), Console.WriteLine);
                        throw;
                    }

                    continue;
                case OpCode.SetTop:
                    // Console.WriteLine($"VM: executing SetTop instruction");
                    // Array.ForEach(Disassembler.Disassemble(Template), Console.WriteLine);
                    if (VARS[IR & 0x00FFFFFFFFFFFFFF] is null) {
                        VARS[IR & 0x00FFFFFFFFFFFFFF] = new Location(Pop());
                        continue;
                    }
                    VARS[IR & 0x00FFFFFFFFFFFFFF].Value = Pop();
                    // Push(VAL = SchemeValue.Void);
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
                    // TODO: check for wrong type
                    Jig.List args = (Jig.List)ENVT.GetArg(0);
                    foreach (var arg in args) {
                        // TODO: type check
                        VAL = (Number)VAL + (Number)arg;
                    }
                    Push(VAL);
                    continue;
                case OpCode.Product:
                    VAL = Integer.One;
                    Jig.List productArgs = (Jig.List)ENVT.GetArg(0);
                    foreach (var arg in productArgs) {
                        // TODO: type check
                        VAL = (Number)VAL * (Number)arg;
                    }
                    Push(VAL);
                    continue;
                case OpCode.ArgToArgs:
                    var tmp = ENVT.GetArg(IR & 0x00FFFFFFFFFFFFFF);
                    if (tmp is not IEnumerable<ISchemeValue> ys) {
                        throw new Exception($"expected list argument but got {tmp}");
                    }
                    foreach (var form in ys.Reverse<ISchemeValue>()) {
                        // TODO: barf
                        Push((SchemeValue)form);
                    }
                    continue;
                case OpCode.Halt:
                    // if we're here then we've popped the top level continuation
                    // Console.WriteLine($"Toplevel! sp = {SP}");
                    var results = new System.Collections.Generic.List<SchemeValue>();
                    while (SP > 0) {
                        results.Add(Pop());
                    }
                    // TODO: this procedure could be a property of the VM rather than the continuation ...
                    ((TopLevelTemplateContinuation)CONT).Procedure(results.ToArray());
                    // TODO: make this the only return statement in the loop
                    return;
                default: throw new Exception($"unhandled case {opCode} in Execute");
            }
        }

    }

    public void Load(Template program, Environment env, ContinuationAny cont) {
        PC = 0;
        var proc = new Procedure(env, program);
        Template = proc.Template;
        VARS = proc.Locations;
        ClearStack();
        ENVT = env;
        CONT = new TopLevelTemplateContinuation(cont);
    }


    public IRuntimeEnvironment RuntimeEnvironment {
        get => ENVT;
        private init => ENVT = (Environment)value;
    }

    public IEnumerable<(Symbol, IExpansionRule)> CoreSyntax { get; }

    private static void TopLevelContinuation(params SchemeValue[] forms) {
        foreach (var form in forms) {
            if (form is not SchemeValue.VoidType) {
                File.AppendAllLines("/home/dave/lan/projects/Jig/log.txt", [form.Print()]);
                Console.WriteLine(form.Print());
            }
        }
    }

    public SyntaxEnvironment FreshCoreSyntax()
    {
        var coreForms = new Dictionary<Symbol, IExpansionRule>();
        // TODO: these should be identifiers, not symbols, but they need to be resolved correctly
        // TODO: probably we don't want to create new identifiers and symbols ...
        // TODO: the ids should have source locations -- names not rows and columns
        coreForms.Add(new Symbol("begin"), new CoreSyntaxRule(CoreParseRules.ParseBeginForm));
        coreForms.Add(new Symbol("define"), new CoreSyntaxRule(CoreParseRules.ParseDefineForm));
        coreForms.Add(new Symbol("define-syntax"), new CoreSyntaxRule(CoreParseRules.DefineSyntax));
        coreForms.Add(new Symbol("if"), new CoreSyntaxRule(CoreParseRules.ParseIfForm));
        coreForms.Add(new Symbol("lambda"), new CoreSyntaxRule(CoreParseRules.ParseLambdaForm));
        coreForms.Add(new Symbol("library"), new CoreSyntaxRule(CoreParseRules.ParseLibraryForm));
        coreForms.Add(new Symbol("quote"), new CoreSyntaxRule(CoreParseRules.ParseQuoteForm));
        coreForms.Add(new Symbol("quote-syntax"), new CoreSyntaxRule(CoreParseRules.ParseQuoteSyntaxForm));
        coreForms.Add(new Symbol("set!"), new CoreSyntaxRule(CoreParseRules.ParseSetForm));
        return new TopLevelSyntaxEnvironment(coreForms);
    }
}