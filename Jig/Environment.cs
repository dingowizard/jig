namespace Jig;

public class Environment : IEnvironment {

    public Environment() {
        _dict.Add(new Expr.Symbol("car"), new Procedure((Builtin) Builtins.car));
        _dict.Add(new Expr.Symbol("cdr"), new Procedure((Builtin) Builtins.cdr));
        _dict.Add(new Expr.Symbol("cons"), new Procedure((Builtin) Builtins.cons));
        _dict.Add(new Expr.Symbol("null?"), new Procedure((Builtin) Builtins.nullP));
        _dict.Add(new Expr.Symbol("succ"), new Procedure((Builtin) Builtins.succ));
        _dict.Add(new Expr.Symbol("number?"), new Procedure((Builtin) Builtins.number_p));
        _dict.Add(new Expr.Symbol("+"), new Procedure((Builtin) Builtins.sum));
        _dict.Add(new Expr.Symbol("*"), new Procedure((Builtin) Builtins.new_product));
        _dict.Add(new Expr.Symbol("-"), new Procedure((PairFunction) Builtins.diff));
        _dict.Add(new Expr.Symbol("="), new Procedure((PairFunction) Builtins.numEq));
        _dict.Add(new Expr.Symbol(">"), new Procedure( (Builtin)Builtins.gt));
        _dict.Add(new Expr.Symbol("apply"), new Procedure( Builtins.apply));
        _dict.Add(new Expr.Symbol("call/cc"), new Procedure( (Builtin)Builtins.callcc));
        _dict.Add(new Expr.Symbol("call-with-current-continuation"), new Procedure( (Builtin)Builtins.callcc));
        _dict.Add(new Expr.Symbol("call-with-values"), new Procedure( Continuation.call_with_values));
        _dict.Add(new Expr.Symbol("values"), new Procedure( (Builtin)Builtins.values));
        _dict.Add(new Expr.Symbol("syntax->list"), new Procedure( (Builtin)Builtins.syntax_to_list));
        _dict.Add(new Expr.Symbol("syntax?"), new Procedure( (Builtin)Builtins.syntax_p));
        _dict.Add(new Expr.Symbol("datum->syntax"), new Procedure( (Builtin)Builtins.datum_to_syntax));
        _dict.Add(new Expr.Symbol("expand"), new Procedure( (Builtin)Builtins.expand));
        _dict.Add(new Expr.Symbol("expand-once"), new Procedure( (Builtin)Builtins.expand_once));
        _dict.Add(new Expr.Symbol("pair?"), new Procedure( (Builtin)Builtins.pair_p));
        _dict.Add(new Expr.Symbol("symbol=?"), new Procedure( (Builtin)Builtins.symbol_equal_p));
        _dict.Add(new Expr.Symbol("symbol?"), new Procedure( (Builtin)Builtins.symbol_p));
        _dict.Add(new Expr.Symbol("symbol->string"), new Procedure( (Builtin)Builtins.symbol_to_string));
        _dict.Add(new Expr.Symbol("string->symbol"), new Procedure( (Builtin)Builtins.string_to_symbol));
        _dict.Add(new Expr.Symbol("syntax-e"), new Procedure( (Builtin)Builtins.syntax_e));
        _dict.Add(new Expr.Symbol("char?"), new Procedure( (Builtin)Builtins.char_p));
        _dict.Add(new Expr.Symbol("eq?"), new Procedure( (Builtin)Builtins.eq_p));
        _dict.Add(new Expr.Symbol("display"), new Procedure( (Builtin)Builtins.display));
        _dict.Add(new Expr.Symbol("newline"), new Procedure( (Builtin)Builtins.newline));
        _dict.Add(new Expr.Symbol("error"), new Procedure( (Builtin)Builtins.error));
        _dict.Add(new Expr.Symbol("vector"), new Procedure( (Builtin)Builtins.vector));
        _dict.Add(new Expr.Symbol("vector?"), new Procedure( (Builtin)Builtins.vector_p));
        _dict.Add(new Expr.Symbol("vector-length"), new Procedure( (Builtin)Builtins.vector_length));
        _dict.Add(new Expr.Symbol("vector-ref"), new Procedure( (Builtin)Builtins.vector_ref));
        _dict.Add(new Expr.Symbol("append"), new Procedure( (Builtin)Builtins.append));
        _dict.Add(new Expr.Symbol("make-record-type-descriptor"), new Procedure( (Builtin)Builtins.make_record_type_descriptor));
        _dict.Add(new Expr.Symbol("make-record-constructor-descriptor"), new Procedure( (Builtin)Builtins.make_record_constructor_descriptor));
        _dict.Add(new Expr.Symbol("record?"), new Procedure( (Builtin)Builtins.record_p));
        _dict.Add(new Expr.Symbol("record-predicate"), new Procedure( (Builtin)Builtins.record_predicate));
        _dict.Add(new Expr.Symbol("record-accessor"), new Procedure( (Builtin)Builtins.record_accessor));
        _dict.Add(new Expr.Symbol("record-constructor"), new Procedure( (Builtin)Builtins.record_constructor));
        // _dict.Add(new Expr.Symbol("dynamic-wind"), new Procedure( (Builtin)Builtins.dynamic_wind));
        // _dict.Add(new Expr.Symbol("error"), new Procedure( (Builtin)Builtins.error));
    }

    public IEnumerable<Expr.Symbol> Symbols {

        get {
            return _dict.Keys;
        }

    }

    public Expr this[Expr.Symbol symbol] {
        get {
            return _dict[symbol];
        }
    }

    public Thunk? Set(Delegate k, Expr sym, Expr v) {
        Syntax.Identifier? id = sym as Syntax.Identifier;
        Expr.Symbol s = id is not null ? id.Symbol : (Expr.Symbol) sym;
        if (!_dict.ContainsKey(s)) {
            throw new Exception($"unbound variable: {s.Name} {(id is not null ? id.SrcLoc.ToString() : "")}");
        }
        _dict[s] = v;
        return Continuation.ApplyDelegate(k, Expr.Void);

    }

    public Thunk? Define (Delegate k, Expr sym, Expr v) {
        Expr.Symbol s = sym is Syntax.Identifier i ? i.Symbol : ((Expr.Symbol) sym);
        if (_dict.ContainsKey(s)) {
            _dict[s] = v;
            return Continuation.ApplyDelegate(k, Expr.Void);
        }
        _dict.Add(s, v);
        return Continuation.ApplyDelegate(k, Expr.Void);
    }

    public Thunk? LookUp (Delegate k, Expr expr) {
        Syntax.Identifier? id = expr as Syntax.Identifier;
        Expr.Symbol symbol = id is not null ? id.Symbol : (Expr.Symbol) expr;
        if (_dict.TryGetValue(symbol, out Expr? result)) {
            return Continuation.ApplyDelegate(k, result);
        } else {
            throw new Exception($"unbound variable: {symbol.Name} {(id is not null ? id.SrcLoc.ToString() : "")}");
        }
    }

    Dictionary<Expr.Symbol, Expr> _dict = new Dictionary<Expr.Symbol,Expr>();

}
