using Jig;
using Jig.Expansion;
using Jig.IO;
using Jig.Reader;
namespace VM;


public class Machine : IRuntime
{

    internal bool Loud = false;

    public Machine(IEvaluator evaluator, Environment env, ContinuationAny cont, uint stackSize = 512) {
        _stackSize = stackSize;
        Evaluator = evaluator;
        ENVT = env;
        VARS = [];
        CONT = new TopLevelContinuation(cont);
        RuntimeEnvironment = ENVT;
        CoreSyntax = SyntaxEnvironment.Default; // TODO: give Machine its own 
        Stack = new SchemeValue[stackSize];
        Template = Template.Empty;
    }

    public Machine(IEvaluator evaluator, Environment env, uint stackSize = 512) {
        _stackSize = stackSize;
        Evaluator = evaluator;
        ENVT = env;
        VARS = [];
        CONT = new TopLevelContinuation(TopLevelContinuation);
        RuntimeEnvironment = ENVT;
        CoreSyntax = SyntaxEnvironment.Default; // TODO: give Machine its own 
        Stack = new SchemeValue[stackSize];
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
            // Console.WriteLine($"\tabout to extend Env with {proc.Template.NumVarsForScope} scope vars");
            // Console.WriteLine("Template:");
            // Array.ForEach(Disassembler.Disassemble(proc.Template), Console.WriteLine);
            ENVT = proc.Environment.Extend(proc.Template.NumVarsForScope);
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
                    if (VAL is Primitive primitiveProc)
                    {
                        primitiveProc.Apply(this);
                        // NOTE: Primitive2 pushes it's result
                        // TODO: grrr. do I have to repeat this business below?
                        /*
                        if (CONT is TopLevelContinuation) {
                            return;
                        }
                        continue;
                    */
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
                        VARS,
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
                    ulong parameterNumber = (IR & 0x00000000FFFFFFFF);
                    for (ulong n = 0; n < parameterNumber; n++) {
                        try
                        {
                            ENVT.BindParameter(n, Pop());
                        }
                        catch (Exception exc) {
                            Console.WriteLine($"VM @ {PC - 1}: trying to bind parameter in template:");
                            Array.ForEach(Disassembler.Disassemble(Template), Console.WriteLine);
                            Console.WriteLine($"Stack is {StackToList()}");
                            
                        }
                    }
                    continue;
                case OpCode.BindRest:
                    // bind zero should always be called first
                    ulong restIndex = (IR & 0x00FFFFFFFFFFFFFF);
                    // TODO:
                    var xs = new System.Collections.Generic.List<SchemeValue>();
                    while (SP != FP) {
                        xs.Add(Pop());
                    }
                    ENVT.BindParameter(restIndex, xs.ToJigList());
                    continue;
                case OpCode.Arg:
                    VAL = ENVT.GetArg(IR & 0x00FFFFFFFFFFFFFF);
                    continue;
                case OpCode.Top:
                    try
                    {
                        VAL = VARS[IR & 0x00FFFFFFFFFFFFFF].Value;
                    }
                    catch (Exception exc)
                    {
                        Console.WriteLine($"PC = {PC}");
                        Console.WriteLine($"ARGS count = {ENVT.ArgsLength}. index was {IR & 0x00FFFFFFFFFFFFFF}");
                        Array.ForEach(Disassembler.Disassemble(Template), Console.WriteLine);
                        throw;
                    }
                    continue;
                case OpCode.Lex:
                    // TODO: no need for two opcodes if code is the same?
                    // int index = (int)(IR & 0x00000000FFFFFFFF);
                    // int depth = (int)(IR >> 32) & 0x00FFFFFF ;
                    VAL = VARS[IR & 0x00FFFFFFFFFFFFFF].Value;
                    continue;
                case OpCode.SetArg:
                    try
                    {
                        ENVT.SetArg(IR & 0x00FFFFFFFFFFFFFF, VAL = Pop());
                    }
                    catch (Exception exc)
                    {
                        Console.WriteLine($"PC = {PC}");
                        Console.WriteLine($"ARGS count = {ENVT.ArgsLength}. index was {IR & 0x00FFFFFFFFFFFFFF}");
                        Array.ForEach(Disassembler.Disassemble(Template), Console.WriteLine);
                        throw;
                    }
                    continue;
                case OpCode.SetLex:
                    // int x = (int)(IR & 0x00000000FFFFFFFF);
                    // int h = (int)(IR >> 32) & 0x00FFFFFF ;
                    try
                    {
                        VARS[IR & 0x00FFFFFFFFFFFFFF].Value = VAL;
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
                    VARS[IR & 0x00FFFFFFFFFFFFFF].Value = VAL;
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
                case OpCode.ArgToArgs:
                    var tmp = Pop();
                    if (tmp is not IEnumerable<ISchemeValue> ys) {
                        throw new Exception($"expected list argument but got {tmp}");
                    }
                    foreach (var form in ys.Reverse<ISchemeValue>()) {
                        // TODO: barf
                        Push((SchemeValue)form);
                    }
                    continue;
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
        CONT = new TopLevelContinuation(cont);
    }


    public Syntax ApplyTransformer(Jig.Expansion.Transformer transformer, Syntax syntax) {
        // return transformer.Apply(syntax);
        throw new NotImplementedException();
    }


    public IRuntimeEnvironment RuntimeEnvironment {
        get => ENVT;
        private init => ENVT = (Environment)value;
    }

    public SyntaxEnvironment CoreSyntax { get; }

    public void Eval(Syntax stx, VM.Environment? env = null) {
        env ??= ENVT;
        
        // var me = new Jig.MacroExpander();
        // Jig.ParsedExpr program = me.Expand(stx, ExEnv);
        // var context = new ExpansionContext(vm, DefaultExpander);
        var program = Evaluator.Expander.ExpandREPLForm(stx, new ExpansionContext(Evaluator, env.TopLevels.Keys));
        
        var compiler = new Compiler(); // should class be static?
        var code = compiler.CompileExprForREPL(program, env);
        // TODO: if we load a different environment here, doesn't that clobber the old one. And then what?
        // NOTE: also a problem in ExecuteFile
        Load(code, env, TopLevelContinuation);
        Run();
    }
    
    
    public void ExecuteFile(string path, Machine vm, VM.Environment? topLevel = null)
    {
        topLevel ??= ENVT;
        InputPort port = new InputPort(path);
        // Continuation.ContinuationAny throwAwayResult = (xs) => null;
        var datums = Reader.ReadFileSyntax(port);
        var parsedProgram = Evaluator.Expander.ExpandFile(datums, new ExpansionContext(Evaluator, topLevel.TopLevels.Keys));
        var compiler = new Compiler();
        var compiled = compiler.CompileFile(parsedProgram.ToArray(), topLevel);
        vm.Load(compiled, topLevel, TopLevelContinuation);
        vm.Run();
    }

    private static void TopLevelContinuation(params SchemeValue[] forms) {
        foreach (var form in forms) {
            if (form is not SchemeValue.VoidType) Console.WriteLine(form.Print());
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
        coreForms.Add(new Symbol("quote"), new CoreSyntaxRule(CoreParseRules.ParseQuoteForm));
        coreForms.Add(new Symbol("quote-syntax"), new CoreSyntaxRule(CoreParseRules.ParseQuoteSyntaxForm));
        coreForms.Add(new Symbol("set!"), new CoreSyntaxRule(CoreParseRules.ParseSetForm));
        return new TopLevelSyntaxEnvironment(coreForms);
    }
}