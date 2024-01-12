namespace Jig;

public class Environment : IEnvironment {

    public Environment() {
        _dict.Add(new Expr.Symbol("car"), new Procedure((Builtin) Builtins.car));
        _dict.Add(new Expr.Symbol("cdr"), new Procedure((Builtin) Builtins.cdr));
        _dict.Add(new Expr.Symbol("cons"), new Procedure((Builtin) Builtins.cons));
        _dict.Add(new Expr.Symbol("null?"), new Procedure((Builtin) Builtins.nullP));
        _dict.Add(new Expr.Symbol("succ"), new Procedure((Builtin) Builtins.succ));
        _dict.Add(new Expr.Symbol("+"), new Procedure((Builtin) Builtins.sum));
        _dict.Add(new Expr.Symbol("*"), new Procedure((Builtin) Builtins.product));
        _dict.Add(new Expr.Symbol("-"), new Procedure((PairFunction) Builtins.diff));
        _dict.Add(new Expr.Symbol("="), new Procedure((PairFunction) Builtins.numEq));
        _dict.Add(new Expr.Symbol("apply"), new Procedure( Builtins.apply));
        // _dict.Add(new Expr.Symbol("call/cc"), new Procedure( (Builtin)Builtins.callcc));
        // _dict.Add(new Expr.Symbol("call-with-values"), new Procedure( Continuation.call_with_values));
        // _dict.Add(new Expr.Symbol("values"), new Procedure( (Builtin)Builtins.values));
        // _dict.Add(new Expr.Symbol("dynamic-wind"), new Procedure( (Builtin)Builtins.dynamic_wind));
        // _dict.Add(new Expr.Symbol("error"), new Procedure( (Builtin)Builtins.error));
    }

    public void Set(Delegate k, Expr sym, Expr v) {
        Expr.Symbol s = sym is SyntaxObject.Identifier i ? i.Symbol : ((Expr.Symbol) sym);
        if (!_dict.ContainsKey(s)) {
            throw new Exception($"set!: unbound variable {s}");
        }
        _dict[s] = v;
        Continuation.ApplyDelegate(k, s);
        return;

    }

    public Continuation.MaybeThunk Define (Delegate k, Expr sym, Expr v) {
        Expr.Symbol s = sym is SyntaxObject.Identifier i ? i.Symbol : ((Expr.Symbol) sym);
        if (_dict.ContainsKey(s)) {
            _dict[s] = v;
            return Continuation.ApplyDelegate(k, s);
        }
        _dict.Add(s, v);
        return Continuation.ApplyDelegate(k, s);
    }

    public Continuation.MaybeThunk LookUp (Delegate k, Expr expr) {
        Expr.Symbol symbol = expr is SyntaxObject.Identifier id ? id.Symbol : (Expr.Symbol) expr;
        if (_dict.TryGetValue(symbol, out Expr? result)) {
            return Continuation.ApplyDelegate(k, result);
        } else {
            throw new Exception($"unbound variable: {symbol.Name}");
        }
    }

    Dictionary<Expr.Symbol, Expr> _dict = new Dictionary<Expr.Symbol,Expr>();

}
