using Jig.IO;

namespace Jig;

public static class Program {

    public static IEnvironment TopLevel = new Environment();

    static void Main(string[] args) {
        IEnvironment topLevel = new Environment();
        // Continuation id = (x) => Console.WriteLine(x.Print());
        Continuation.ContinuationAny print = (Continuation.ContinuationAny)Print;
        ExecuteFile("prelude.scm", topLevel);
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
                    Eval(print, input, topLevel);
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

    public static Thunk? Print(params Expr[] exprs) {
        foreach (var expr in exprs) {
            Console.WriteLine(expr.Print());
        }
        return null;
    }

    public static void Eval(Delegate k, Expr ast, IEnvironment env) {
        if (ast is Syntax stx) {
            ast = new MacroExpander().Expand(stx, ExpansionEnvironment.Default);
        }
        var compiled = Compiler.Compile(ast);
        Run(compiled, k, env);
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
