using Jig.IO;

namespace Jig;

public static class Program {

    public static IEnvironment TopLevel = new Environment();

    static void Main(string[] args) {
        IEnvironment topLevel = new Environment();
        Continuation id = (x) => Console.WriteLine(x.Print());
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
                    Eval(id, input, topLevel);
                } catch (Exception x) {
                    Console.WriteLine(x);
                }
                Console.Write("> ");
            }
        }
    }

    public static object? Run(CompiledCode code) {
        object? result = null;
        Continuation k = (v) => result = v;
        code(k, TopLevel);
        return result;
    }

    public static void Eval(Continuation k, Expr ast, IEnvironment env) {
        var compiled = Compiler.Compile(ast);
        compiled(k,env);
    }

    // public static void Eval(Continuation k, SyntaxObject stx, IEnvironment env) {
    //     var compiled = Compiler.Compile(stx);
    //     compiled(k,env);
    // }


}

public class Environment : IEnvironment {

    public Environment() {
        // TODO: why are we using strings rather than symbols?
        _dict.Add("myVar", new Expr.Integer(12));
        _dict.Add("a", new Expr.Integer(1));
        _dict.Add("b", new Expr.Integer(2));
        _dict.Add("c", new Expr.Integer(3));
        _dict.Add("car", new LiteralExpr<Delegate>((Builtin) Builtins.car));
        _dict.Add("cdr", new LiteralExpr<Delegate>((Builtin) Builtins.cdr));
        _dict.Add("cons", new LiteralExpr<Delegate>((Builtin) Builtins.cons));
        _dict.Add("null?", new LiteralExpr<Delegate>((Builtin) Builtins.nullP));
        _dict.Add("succ", new LiteralExpr<Delegate>((Builtin) Builtins.succ));
        _dict.Add("+", new LiteralExpr<Delegate>((Builtin) Builtins.sum));
        _dict.Add("*", new LiteralExpr<Delegate>((Builtin) Builtins.product));
        _dict.Add("-", new LiteralExpr<Delegate>((PairFunction) Builtins.diff));
        _dict.Add("=", new LiteralExpr<Delegate>((PairFunction) Builtins.numEq));
        // _dict.Add("procedure?", (Builtin) Builtins.procedureP);

    }

    public void Set(Continuation k, Expr sym, Expr v) {
        string name = sym is SyntaxObject.Identifier id ? id.Symbol.Name : ((Expr.Symbol) sym).Name;
        Expr.Symbol s = sym is SyntaxObject.Identifier i ? i.Symbol : ((Expr.Symbol) sym);
        if (!_dict.ContainsKey(name)) {
            throw new Exception($"set!: unbound variable {s}");
        }
        _dict[name] = v;
        k(s);
        return;

    }

    public void Define (Continuation k, Expr sym, Expr v) {
        string name = sym is SyntaxObject.Identifier id ? id.Symbol.Name : ((Expr.Symbol) sym).Name;
        Expr.Symbol s = sym is SyntaxObject.Identifier i ? i.Symbol : ((Expr.Symbol) sym);
        _dict.Add(name, v);
        k(s);
        return;
    }

    public void LookUp (Continuation k, Expr expr) {
        Expr.Symbol symbol = expr is SyntaxObject.Identifier id ? id.Symbol : (Expr.Symbol) expr;
        if (_dict.TryGetValue(symbol.Name, out Expr? result)) {
            k(result);
        } else {
            throw new Exception($"unbound variable: {symbol.Name}");
        }
    }

    // public void LookUpSyntax (Continuation k, SyntaxObject.Identifier id) {
    //     if (_dict.TryGetValue(id.Symbol.Name, out Expr? result)) {
    //         k(result);
    //     } else {
    //         throw new Exception($"unbound variable: {id.Symbol.Name} at {id.SrcLoc}");
    //     }
    // }

    Dictionary<string, Expr> _dict = new Dictionary<string,Expr>();

}
