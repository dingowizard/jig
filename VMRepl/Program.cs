using Jig;
using Jig.IO;
using VM;
using Mono.Options;
using System.Diagnostics;
using Jig.Reader;

public static class Program {

    public static VM.Environment TopLevel = VM.Environment.Default;

    static void Main(string[] args) {

        string debugLogPath = "debug.log";
        TextWriterTraceListener fileListener = new TextWriterTraceListener(debugLogPath);
        Trace.Listeners.Add(fileListener);
        
        string scriptPath = "";
        string expr = "";
        bool showHelp = false;
        bool quiet = false;
        var options = new OptionSet {
            {"s|script=","run script and exit.", s => scriptPath = s},
            {"e|expr=", "evaluate expression and exit", e => expr = e},
            {"h|help", "show this message", h => showHelp = h is not null},
            {"q|quiet", "suppress prompt", q => quiet = q is not null},
        };
        System.Collections.Generic.List<string> extraArgs;
        try {
            extraArgs = options.Parse(args);
        } catch (OptionException xc) {
            Console.Error.Write("jig: ");
            Console.Error.WriteLine(xc.Message);
            Console.Error.WriteLine("Try 'jig --help' for more information");
            System.Environment.Exit(-1);
        }
        if (showHelp) {
            Console.WriteLine("Options:");
            options.WriteOptionDescriptions(Console.Out);
            System.Environment.Exit(0);
        }

        // this is just a kludge while libraries aren't implemented
        // need to have evaluated certain functions in order to
        // expand the rhs of define-syntaxes
        IEvaluator evaluator = new Evaluator();
        ILibrary coreBuiltins = Library.Core;
        
        // InputPort filePort = new InputPort("prelude-1.scm");
        // var stxes = Reader.ReadFileSyntax(filePort);
        // evaluator.EvalSequence(ThrowAwayAny, stxes);
        evaluator.Import(coreBuiltins);
        evaluator.Import(coreBuiltins, 1); // "for syntax"
        var prelude1Lib = Library.FromFile("prelude-1.scm", Reader.ReadFileSyntax, new VMFactory(), [coreBuiltins]);
        var preludeLib = Library.FromFile("prelude.scm", Reader.ReadFileSyntax, new VMFactory(), [coreBuiltins, prelude1Lib]);
        evaluator.Import(prelude1Lib);
        evaluator.Import(prelude1Lib, 1);
        evaluator.Import(preludeLib);
        evaluator.Import(preludeLib, 1);
        // var preludeLib = Library.FromFile("prelude.scm", Reader.ReadFileSyntax, new VMFactory(), [coreBuiltins, prelude1Lib]);
        // evaluator.Import(preludeLib);
        // filePort = new InputPort("prelude.scm");
        // stxes = Reader.ReadFileSyntax(filePort);
        // evaluator.EvalSequence(ThrowAwayAny, stxes);
        
        if (expr != "") {
            using (InputPort port = InputPort.FromString(expr)) {
                Syntax? stx = Reader.ReadSyntax(port);
                if (stx is null) {
                    Console.Error.WriteLine($"failed to read {expr}.");
                    System.Environment.Exit(-1);
                }
                evaluator.Eval(TopLevelContinuation, stx);
            }
            System.Environment.Exit(0);
        
        }
        // REPL
        if (!quiet) {
            Console.Write("> ");
        }
        Syntax? input;
        // try {
        using (InputPort port = new InputPort(Console.In)) {
            while (true) {
                    try {
                        input = Reader.ReadSyntax(port);
                        if (input is null) {
                            if (!quiet) {
                                Console.WriteLine();
                                Console.WriteLine("Goodbye!");
                            }

                            break;
                        }

                        evaluator.Eval(TopLevelContinuation, input);

                    } catch (Exception x) {
                        Console.Error.WriteLine(x);
                    }

                    if (!quiet) {
                        Console.Write("> ");
                    }
                }
            }
    
    }
    // // TODO: should Eval be a method of VM?
    // // TODO: should the toplevel continuation be a field of VM rather than an argument to load?
    // public static void Eval(Syntax stx, VM.Environment? env = null) {
    //     env ??= Program.TopLevel;
    //     
    //     // var me = new Jig.MacroExpander();
    //     // Jig.ParsedExpr program = me.Expand(stx, ExEnv);
    //     // var context = new ExpansionContext(vm, DefaultExpander);
    //     var program = DefaultExpander.ExpandREPLForm(stx, env.GetExpansionContext());
    //     
    //     var compiler = new Compiler(); // should class be static?
    //     var code = compiler.CompileExprForREPL(program, env);
    //     env.Machine.Load(code, env, TopLevelContinuation);
    //     env.Machine.Run();
    // }
    //
    //
    // public static void ExecuteFile(string path, Machine vm, VM.Environment? topLevel = null)
    // {
    //     topLevel = topLevel ?? Program.TopLevel;
    //     InputPort port = new InputPort(path);
    //     // Continuation.ContinuationAny throwAwayResult = (xs) => null;
    //     var datums = Reader.ReadFileSyntax(port);
    //     var parsedProgram = DefaultExpander.ExpandFile(datums, topLevel.GetExpansionContext());
    //     var compiler = new Compiler();
    //     var compiled = compiler.CompileFile(parsedProgram.ToArray(), topLevel);
    //     vm.Load(compiled, topLevel, TopLevelContinuation);
    //     vm.Run();
    // }
    //
    private static void TopLevelContinuation(params Form[] forms) {
        foreach (var form in forms) {
            if (form is not Form.VoidType) Console.WriteLine(form.Print());
        }
    }
    
    private static void ThrowAwayAny(params Form[] args) {}
}
