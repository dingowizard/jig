using Jig.IO;

namespace Jig;

public static class Program {

    public static IEnvironment TopLevel = new Environment();

    static void Main(string[] args) {
        IEnvironment topLevel = new Environment();
        // Continuation id = (x) => Console.WriteLine(x.Print());
        Continuation.ContinuationAny print = (Continuation.ContinuationAny)Print;
        // REPL
        Console.Write("> ");
        SyntaxObject? input;
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

    public static void Print(params Expr[] exprs) {
        foreach (var expr in exprs) {
            Console.WriteLine(expr.Print());
        }
    }

    public static void Eval(Delegate k, Expr ast, IEnvironment env) {
        var compiled = Compiler.Compile(ast);
        compiled(k,env);
    }

    public static void Run(CompiledCode code, Delegate k, IEnvironment env) {
        code(k, env);
        return;
    }


}
