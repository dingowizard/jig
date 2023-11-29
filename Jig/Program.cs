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

    public static void Eval(Continuation k, Expr ast, IEnvironment env) {
        var compiled = Compiler.Compile(ast);
        compiled(k,env);
    }


}

public class Environment : IEnvironment {

    public Environment() {
        _dict.Add(new Expr.Symbol("car"), new LiteralExpr<Delegate>((Builtin) Builtins.car));
        _dict.Add(new Expr.Symbol("cdr"), new LiteralExpr<Delegate>((Builtin) Builtins.cdr));
        _dict.Add(new Expr.Symbol("cons"), new LiteralExpr<Delegate>((Builtin) Builtins.cons));
        _dict.Add(new Expr.Symbol("null?"), new LiteralExpr<Delegate>((Builtin) Builtins.nullP));
        _dict.Add(new Expr.Symbol("succ"), new LiteralExpr<Delegate>((Builtin) Builtins.succ));
        _dict.Add(new Expr.Symbol("+"), new LiteralExpr<Delegate>((Builtin) Builtins.sum));
        _dict.Add(new Expr.Symbol("*"), new LiteralExpr<Delegate>((Builtin) Builtins.product));
        _dict.Add(new Expr.Symbol("-"), new LiteralExpr<Delegate>((PairFunction) Builtins.diff));
        _dict.Add(new Expr.Symbol("="), new LiteralExpr<Delegate>((PairFunction) Builtins.numEq));
        _dict.Add(new Expr.Symbol("apply"), new LiteralExpr<Delegate>( Builtins.apply));

    }

    public void Set(Continuation k, Expr sym, Expr v) {
        Expr.Symbol s = sym is SyntaxObject.Identifier i ? i.Symbol : ((Expr.Symbol) sym);
        if (!_dict.ContainsKey(s)) {
            throw new Exception($"set!: unbound variable {s}");
        }
        _dict[s] = v;
        k(s);
        return;

    }

    public void Define (Continuation k, Expr sym, Expr v) {
        Expr.Symbol s = sym is SyntaxObject.Identifier i ? i.Symbol : ((Expr.Symbol) sym);
        if (_dict.ContainsKey(s)) {
            _dict[s] = v;
            k(s);
            return;
        }
        _dict.Add(s, v);
        k(s);
        return;
    }

    public void LookUp (Continuation k, Expr expr) {
        Expr.Symbol symbol = expr is SyntaxObject.Identifier id ? id.Symbol : (Expr.Symbol) expr;
        if (_dict.TryGetValue(symbol, out Expr? result)) {
            k(result);
        } else {
            throw new Exception($"unbound variable: {symbol.Name}");
        }
    }

    Dictionary<Expr.Symbol, Expr> _dict = new Dictionary<Expr.Symbol,Expr>();

}
