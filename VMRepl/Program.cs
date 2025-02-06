using Jig;
using Jig.IO;
using VM;

public static class Program {

    public static VM.Environment TopLevel = VM.Environment.Default;
    public static Jig.ExpansionEnvironment ExEnv = Jig.ExpansionEnvironment.Default;

    static void Main(string[] args) {
        // string scriptPath = "";
        string expr = "";
        // bool showHelp = false;
        // var options = new OptionSet {
        //     {"s|script=","run script and exit.", s => scriptPath = s},
        //     {"e|expr=", "evaluate expression and exit", e => expr = e},
        //     {"h|help", "show this message", h => showHelp = h is not null},
        // };
        // System.Collections.Generic.List<string> extraArgs;
        // try {
        //     extraArgs = options.Parse(args);
        // } catch (OptionException x) {
        //     Console.Error.Write("jig: ");
        //     Console.Error.WriteLine(x.Message);
        //     Console.Error.WriteLine("Try 'jig --help' for more information");
        //     System.Environment.Exit(-1);
        // }
        // if (showHelp) {
        //     Console.WriteLine("Options:");
        //     options.WriteOptionDescriptions(Console.Out);
        //     System.Environment.Exit(0);
        // }
        //
        // ExecuteFile("prelude.scm", TopLevel);

        // if (scriptPath != "") {
        //     ExecuteFile(scriptPath, TopLevel);
        //     System.Environment.Exit(0);
        //
        // }

        // Continuation.ContinuationAny print = (Continuation.ContinuationAny)Print;
        // if (expr != "") {
        //     using (InputPort port = InputPort.FromString(expr)) {
        //         Syntax? stx = Jig.Reader.Reader.ReadSyntax(port);
        //         if (stx is null) {
        //             Console.Error.WriteLine($"failed to read {expr}.");
        //             System.Environment.Exit(-1);
        //         }
        //         Eval(print, stx, TopLevel);
        //     }
        //     System.Environment.Exit(0);
        //
        // }
        // REPL
        Console.Write("> ");
        Jig.Syntax? input;
        VM.Machine vm = new VM.Machine();
        while (true) {
            using (InputPort port = new InputPort(Console.In)) {
                try {
                    input = Jig.Reader.Reader.ReadSyntax(port);
                    if (input is null) {
                        Console.WriteLine();
                        Console.WriteLine("Goodbye!");
                        break;
                    }

                    Jig.Form form = Eval(vm, input, TopLevel);
                    if (form is null) {
                        Console.WriteLine("form was null!");
                    }
                    if (form is not Form.VoidType) {
                        Console.WriteLine(form.Print());
                    }
                }
                catch (Exception x) {
                    Console.WriteLine(x);
                }

                Console.Write("> ");
            }
        }
    }
    public static Jig.Form Eval(Machine vm, Jig.Syntax stx, VM.Environment? env = null) {
        env ??= Program.TopLevel;
        var me = new Jig.MacroExpander();
        Jig.ParsedExpr program = me.Expand(stx, ExEnv);
        var compiler = new VM.Compiler(); // should class be static?
        var ctEnv = new CompileTimeEnvironment(me.Bindings, env);
        var code = compiler.CompileExprForREPL(program, ctEnv);
        vm.Load(code, env);
        return vm.Run();
    }
}
