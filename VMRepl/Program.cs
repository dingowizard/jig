using Jig;
using Jig.IO;
using VM;
using Mono.Options;
using System.Diagnostics;
using Jig.Expansion;
using Jig.Reader;
using Environment = Jig.Environment;

public static class Program {

    public static VM.Environment TopLevel = VM.Environment.Default;
    public static Jig.Expansion.Expander DefaultExpander = new Jig.Expansion.Expander(); 

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
        
        VM.Machine vm = new VM.Machine();
        ExecuteFile("prelude.scm", vm, TopLevel);
        
        if (expr != "") {
            using (InputPort port = InputPort.FromString(expr)) {
                Syntax? stx = Jig.Reader.Reader.ReadSyntax(port);
                if (stx is null) {
                    Console.Error.WriteLine($"failed to read {expr}.");
                    System.Environment.Exit(-1);
                }
                Eval(stx, TopLevel);
            }
            System.Environment.Exit(0);
        
        }
        // REPL
        if (!quiet) {
            Console.Write("> ");
        }
        Jig.Syntax? input;
        // try {
        using (InputPort port = new InputPort(Console.In)) {
            while (true) {
                    try {
                        input = Jig.Reader.Reader.ReadSyntax(port);
                        if (input is null) {
                            if (!quiet) {
                                Console.WriteLine();
                                Console.WriteLine("Goodbye!");
                            }

                            break;
                        }

                        Eval(input, TopLevel);

                    } catch (Exception x) {
                        Console.Error.WriteLine(x);
                    }

                    if (!quiet) {
                        Console.Write("> ");
                    }
                }
            }
    
    }
    // TODO: should Eval be a method of VM?
    // TODO: should the toplevel continuation be a field of VM rather than an argument to load?
    public static void Eval(Jig.Syntax stx, VM.Environment? env = null) {
        env ??= Program.TopLevel;
        
        // var me = new Jig.MacroExpander();
        // Jig.ParsedExpr program = me.Expand(stx, ExEnv);
        // var context = new ExpansionContext(vm, DefaultExpander);
        var program = DefaultExpander.Expand(stx, env.GetExpansionContext());
        
        var compiler = new VM.Compiler(); // should class be static?
        var ctEnv = new CompileTimeEnvironment(env); // TODO: why does the cte need these bindings?
        var code = compiler.CompileExprForREPL(program, ctEnv);
        env.Machine.Load(code, env, TopLevelContinuation);
        env.Machine.Run();
    }
    
    
    public static void ExecuteFile(string path, Machine vm, VM.Environment? topLevel = null)
    {
        topLevel = topLevel ?? Program.TopLevel;
        InputPort port = new InputPort(path);
        // Continuation.ContinuationAny throwAwayResult = (xs) => null;
        var datums = Reader.ReadFileSyntax(port);
        var parsedProgram = DefaultExpander.ExpandFile(datums, topLevel.GetExpansionContext());
        var compiler = new VM.Compiler();
        var cte = new CompileTimeEnvironment(topLevel);
        var compiled = compiler.CompileFile(parsedProgram.ToArray(), cte);
        vm.Load(compiled, topLevel, TopLevelContinuation);
        vm.Run();
    }

    private static void TopLevelContinuation(params Form[] forms) {
        foreach (var form in forms) {
            if (form is not Form.VoidType) Console.WriteLine(form.Print());
        }
        
    }
    private static void ThrowAway(params Form[] forms) {
        return;

    }
}
