using Jig.IO;
using Mono.Options;

namespace Jig;

public static class Program {

    public static IEnvironment TopLevel = new Environment();
    public static ExpansionEnvironment ExEnv = ExpansionEnvironment.Default;

    static void Main(string[] args) {
        string scriptPath = "";
        string expr = "";
        bool showHelp = false;
        var options = new OptionSet {
            {"s|script=","run script and exit.", s => scriptPath = s},
            {"e|expr=", "evaluate expression and exit", e => expr = e},
            {"h|help", "show this message", h => showHelp = h is not null},
        };
        System.Collections.Generic.List<string> extraArgs;
        try {
            extraArgs = options.Parse(args);
        } catch (OptionException x) {
            Console.Error.Write("jig: ");
            Console.Error.WriteLine(x.Message);
            Console.Error.WriteLine("Try 'jig --help' for more information");
            System.Environment.Exit(-1);
        }
        if (showHelp) {
            Console.WriteLine("Options:");
            options.WriteOptionDescriptions(Console.Out);
            System.Environment.Exit(0);
        }

        ExecuteFile("prelude.scm", TopLevel);

        if (scriptPath != "") {
            ExecuteFile(scriptPath, TopLevel);
            System.Environment.Exit(0);

        }

        Continuation.ContinuationAny print = (Continuation.ContinuationAny)Print;
        if (expr != "") {
            using (InputPort port = InputPort.FromString(expr)) {
                Syntax? stx = Jig.Reader.Reader.ReadSyntax(port);
                if (stx is null) {
                    Console.Error.WriteLine($"failed to read {expr}.");
                    System.Environment.Exit(-1);
                }
                Eval(print, stx, TopLevel);
            }
            System.Environment.Exit(0);

        }
        // REPL
        Console.Write("> ");
        Syntax? input;
        while (true) {
            using (InputPort port = new InputPort(Console.In)) {
                try {
                    input = Jig.Reader.Reader.ReadSyntax(port);
                    if (input is null) {
                        Console.WriteLine();
                        Console.WriteLine("Goodbye!");
                        break;
                    }
                    Eval(print, input, TopLevel);
                } catch (Exception x) {
                    Console.WriteLine(x);
                }
                Console.Write("> ");
            }
        }
    }

    public static void ExecuteFile(string path, IEnvironment topLevel)
    {
        InputPort port = new InputPort(path);
        Continuation.ContinuationAny throwAwayResult = (xs) => null;
        Syntax? x = Jig.Reader.Reader.ReadSyntax(port);
        while (x is not null) {
            Eval(throwAwayResult, x, topLevel);
            x = Jig.Reader.Reader.ReadSyntax(port);
        }
    }

    public static Thunk? Print(params IForm[] exprs) {
        foreach (var expr in exprs) {
            if (expr is not Form.VoidType) {
                Console.WriteLine(expr.Print());
            }
        }
        return null;
    }

    public static void Eval(Delegate k, IForm ast, IEnvironment? env = null) {
        // TODO: should eval return Thunk??
        env ??= Program.TopLevel;
        if (ast is Syntax stx) {
            // TODO: shouldn't we be able to macro-expand syntactic-data as well as syntax objects?
            ParsedExpr program = new MacroExpander().Expand(stx, ExEnv);
            var scope = new LexicalContext();
            var c = ET.Analyze(scope, program).Compile();
            Run(c, k, env);
            return;
        }
        var compiled = Compiler.Compile(ast);
        Run(compiled, k, env);
    }

    public static IForm EvalNonCPS(IForm ast, IEnvironment? env = null) {
        IForm? expr = null;
        Continuation.OneArgDelegate setResult = (x) => {expr = x; return null;};
        Eval(setResult, ast, env);

        if (expr is null) throw new Exception();
        return expr;
    }

    public static IForm EvalNonCPSNoExpand(Form ast, IEnvironment? env = null) {
        IForm? expr = null;
        Continuation.OneArgDelegate setResult = (x) => {expr = x; return null;};
        var compiled = Compiler.Compile(ast);
        Run(compiled, setResult, env ?? Program.TopLevel);
        if (expr is null) throw new Exception();
        return expr;
    }

    public static void Run(CompiledCode code, Delegate k, IEnvironment env) {
        // trampoline
        Thunk? thunk = code(k, env);
        while (thunk is not null) {
            // Console.WriteLine("trampoline: Bounce!");
            thunk = thunk();
        }
    }

}
