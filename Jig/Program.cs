using Jig.IO;

namespace Jig;

public static class Program {

    public static IEnvironment TopLevel = new Environment();

    static void Main(string[] args) {
        IEnvironment topLevel = new Environment();
        // Continuation id = (x) => Console.WriteLine(x.Print());
        ContinuationAny print = (ContinuationAny)Print;
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
        _dict.Add(new Expr.Symbol("call/cc"), new LiteralExpr<Delegate>( (Builtin)Builtins.callcc));
        _dict.Add(new Expr.Symbol("call-with-values"), new LiteralExpr<Delegate>( Builtins.call_with_values));
        _dict.Add(new Expr.Symbol("values"), new LiteralExpr<Delegate>( (Builtin)Builtins.values));

    }

    public void Set(Delegate k, Expr sym, Expr v) {
        Expr.Symbol s = sym is SyntaxObject.Identifier i ? i.Symbol : ((Expr.Symbol) sym);
        if (!_dict.ContainsKey(s)) {
            throw new Exception($"set!: unbound variable {s}");
        }
        _dict[s] = v;
        Builtins.ApplyContinuation(k, s);
        return;

    }

    public void Define (Delegate k, Expr sym, Expr v) {
        Expr.Symbol s = sym is SyntaxObject.Identifier i ? i.Symbol : ((Expr.Symbol) sym);
        if (_dict.ContainsKey(s)) {
            _dict[s] = v;
            Builtins.ApplyContinuation(k, s);
            return;
        }
        _dict.Add(s, v);
        Builtins.ApplyContinuation(k, s);
        return;
    }

    public void LookUp (Delegate k, Expr expr) {
        Expr.Symbol symbol = expr is SyntaxObject.Identifier id ? id.Symbol : (Expr.Symbol) expr;
        if (_dict.TryGetValue(symbol, out Expr? result)) {
            Builtins.ApplyContinuation(k, result);
            return;
        } else {
            throw new Exception($"unbound variable: {symbol.Name}");
        }
    }

    Dictionary<Expr.Symbol, Expr> _dict = new Dictionary<Expr.Symbol,Expr>();

}
