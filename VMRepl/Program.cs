using Jig;
using Jig.IO;
using VM;
using Mono.Options;
using System.Diagnostics;

public static class Program {

    public static VM.Environment TopLevel = VM.Environment.Default;
    public static Jig.ExpansionEnvironment ExEnv = Jig.ExpansionEnvironment.Default;

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
        if (expr != "") {
            using (InputPort port = InputPort.FromString(expr)) {
                Syntax? stx = Jig.Reader.Reader.ReadSyntax(port);
                if (stx is null) {
                    Console.Error.WriteLine($"failed to read {expr}.");
                    System.Environment.Exit(-1);
                }
                Eval(vm, stx, TopLevel);
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

                        Eval(vm, input, TopLevel);
                        vm.DoResults(f => Console.WriteLine(f.Print()));

                    } catch (Exception x) {
                        Console.Error.WriteLine(x);
                    }

                    if (!quiet) {
                        Console.Write("> ");
                    }
                }
            }
    
    }
    public static void Eval(Machine vm, Jig.Syntax stx, VM.Environment? env = null) {
        env ??= Program.TopLevel;
        var me = new Jig.MacroExpander();
        Jig.ParsedExpr program = me.Expand(stx, ExEnv);
        var compiler = new VM.Compiler(); // should class be static?
        var ctEnv = new CompileTimeEnvironment(me.Bindings, env);
        var code = compiler.CompileExprForREPL(program, ctEnv);
        vm.Load(code, env);
        vm.Run();
    }
}
