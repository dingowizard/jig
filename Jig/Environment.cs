namespace Jig;

public class Environment : IEnvironment {

    private readonly Environment? Parent;

    public Environment Import(Library library) {
        var env = new Environment(this.Parent);
        return env.Extend(library.Exports);

    }

    public Environment Extend(IEnumerable<Tuple<Form.Symbol, Form>> bindings) {
        
        var result = new Environment(this.Parent);
        foreach (var pair in bindings) {
            result._dict.Add(pair.Item1, pair.Item2);
        }
        return result;
    }

    private Environment(Environment? parent) {
        Parent = parent;
    }

    public static Environment Default {
        get {
            var env = new Environment();
            env._dict.Add(new Form.Symbol("car"), new Procedure((Builtin) Builtins.car));
            env._dict.Add(new Form.Symbol("cdr"), new Procedure((Builtin) Builtins.cdr));
            env._dict.Add(new Form.Symbol("cons"), new Procedure((Builtin) Builtins.cons));
            env._dict.Add(new Form.Symbol("null?"), new Procedure((Builtin) Builtins.nullP));
            env._dict.Add(new Form.Symbol("succ"), new Procedure((Builtin) Builtins.succ));
            env._dict.Add(new Form.Symbol("number?"), new Procedure((Builtin) Builtins.number_p));
            env._dict.Add(new Form.Symbol("integer?"), new Procedure((Builtin) Number.integer_p));
            env._dict.Add(new Form.Symbol("real?"), new Procedure((Builtin) Number.real_p));
            env._dict.Add(new Form.Symbol("procedure?"), new Procedure((Builtin) Procedure.procedure_p));
            env._dict.Add(new Form.Symbol("+"), new Procedure((Builtin) Builtins.sum));
            env._dict.Add(new Form.Symbol("*"), new Procedure((Builtin) Number.product));
            env._dict.Add(new Form.Symbol("/"), new Procedure((PairFunction) Number.divide));
            env._dict.Add(new Form.Symbol("-"), new Procedure((PairFunction) Number.diff));
            env._dict.Add(new Form.Symbol(">"), new Procedure( (Builtin)Number.gt));
            env._dict.Add(new Form.Symbol(">="), new Procedure( (Builtin)Number.gte));
            env._dict.Add(new Form.Symbol("<"), new Procedure( (Builtin)Number.lt));
            env._dict.Add(new Form.Symbol("<="), new Procedure( (Builtin)Number.lte));
            env._dict.Add(new Form.Symbol("mod"), new Procedure( (Builtin)Number.mod));
            env._dict.Add(new Form.Symbol("floor"), new Procedure( (Builtin)Number.floor));
            env._dict.Add(new Form.Symbol("ceiling"), new Procedure( (Builtin)Number.ceiling));
            env._dict.Add(new Form.Symbol("apply"), new Procedure( Builtins.apply));
            env._dict.Add(new Form.Symbol("eval"), new Procedure( (Builtin)Builtins.eval));
            env._dict.Add(new Form.Symbol("call/cc"), new Procedure( (Builtin)Builtins.callcc));
            env._dict.Add(new Form.Symbol("call-with-current-continuation"), new Procedure( (Builtin)Builtins.callcc));
            env._dict.Add(new Form.Symbol("call-with-values"), new Procedure( Continuation.call_with_values));
            env._dict.Add(new Form.Symbol("values"), new Procedure( (Builtin)Builtins.values));
            env._dict.Add(new Form.Symbol("syntax->list"), new Procedure( (Builtin)Builtins.syntax_to_list));
            env._dict.Add(new Form.Symbol("syntax?"), new Procedure( (Builtin)Builtins.syntax_p));
            env._dict.Add(new Form.Symbol("datum->syntax"), new Procedure( (Builtin)Builtins.datum_to_syntax));
            env._dict.Add(new Form.Symbol("syntax->datum"), new Procedure( (Builtin)Builtins.syntax_to_datum));
            env._dict.Add(new Form.Symbol("expand"), new Procedure( (Builtin)Builtins.expand));
            env._dict.Add(new Form.Symbol("expand-once"), new Procedure( (Builtin)Builtins.expand_once));
            env._dict.Add(new Form.Symbol("pair?"), new Procedure( (Builtin)Builtins.pair_p));
            env._dict.Add(new Form.Symbol("symbol=?"), new Procedure( (Builtin)Builtins.symbol_equal_p));
            env._dict.Add(new Form.Symbol("symbol?"), new Procedure( (Builtin)Builtins.symbol_p));
            env._dict.Add(new Form.Symbol("symbol->string"), new Procedure( (Builtin)Builtins.symbol_to_string));
            env._dict.Add(new Form.Symbol("string->symbol"), new Procedure( (Builtin)Builtins.string_to_symbol));
            env._dict.Add(new Form.Symbol("syntax-e"), new Procedure( (Builtin)Builtins.syntax_e));
            env._dict.Add(new Form.Symbol("char?"), new Procedure( (Builtin)Builtins.char_p));
            env._dict.Add(new Form.Symbol("string?"), new Procedure( (Builtin)String.string_p));
            env._dict.Add(new Form.Symbol("boolean?"), new Procedure( (Builtin)Bool.boolean_p));
            env._dict.Add(new Form.Symbol("boolean=?"), new Procedure( (ImproperListFunction2)Bool.boolean_eq_p));
            env._dict.Add(new Form.Symbol("eq?"), new Procedure( (Builtin)Builtins.eq_p));
            env._dict.Add(new Form.Symbol("eqv?"), new Procedure( (Builtin)Builtins.eqv_p));
            env._dict.Add(new Form.Symbol("="), new Procedure((PairFunction) Builtins.numEq));
            env._dict.Add(new Form.Symbol("display"), new Procedure( (Builtin)Builtins.display));
            env._dict.Add(new Form.Symbol("newline"), new Procedure( (Builtin)Builtins.newline));
            env._dict.Add(new Form.Symbol("error"), new Procedure( (Builtin)Builtins.error));
            env._dict.Add(new Form.Symbol("vector"), new Procedure( (Builtin)Builtins.vector));
            env._dict.Add(new Form.Symbol("vector?"), new Procedure( (Builtin)Builtins.vector_p));
            env._dict.Add(new Form.Symbol("vector-length"), new Procedure( (Builtin)Builtins.vector_length));
            env._dict.Add(new Form.Symbol("vector-ref"), new Procedure( (Builtin)Builtins.vector_ref));
            env._dict.Add(new Form.Symbol("append"), new Procedure( (Builtin)Builtins.append));
            env._dict.Add(new Form.Symbol("make-record-type-descriptor"), new Procedure( (Builtin)Builtins.make_record_type_descriptor));
            env._dict.Add(new Form.Symbol("record-type-descriptor?"), new Procedure( (Builtin)Builtins.record_type_descriptor_p));
            env._dict.Add(new Form.Symbol("record-constructor-descriptor?"), new Procedure( (Builtin)Builtins.record_constructor_descriptor_p));
            env._dict.Add(new Form.Symbol("make-record-constructor-descriptor"), new Procedure( (Builtin)Builtins.make_record_constructor_descriptor));
            env._dict.Add(new Form.Symbol("record?"), new Procedure( (Builtin)Builtins.record_p));
            env._dict.Add(new Form.Symbol("record-predicate"), new Procedure( (Builtin)Builtins.record_predicate));
            env._dict.Add(new Form.Symbol("record-accessor"), new Procedure( (Builtin)Builtins.record_accessor));
            env._dict.Add(new Form.Symbol("record-constructor"), new Procedure( (Builtin)Builtins.record_constructor));
            env._dict.Add(new Form.Symbol("map"), new Procedure( (Builtin)Builtins.map));
            // _dict.Add(new Expr.Symbol("dynamic-wind"), new Procedure( (Builtin)Builtins.dynamic_wind));
            // _dict.Add(new Expr.Symbol("error"), new Procedure( (Builtin)Builtins.error));
            return env;

        }
    }

    public Environment() {
        Parent = null;
    }

    public IEnumerable<Form.Symbol> Symbols => _dict.Keys;

    public Form this[Form.Symbol symbol] => _dict[symbol];

    public Thunk? Set(Delegate k, Form sym, Form v) {
        Syntax.Identifier? id = sym as Syntax.Identifier;
        Form.Symbol s = id is not null ? id.Symbol : (Form.Symbol) sym;
        if (!_dict.ContainsKey(s)) {
            throw new Exception($"unbound variable: {s.Name} {(id is not null ? id.SrcLoc.ToString() : "")}");
        }
        _dict[s] = v;
        return Continuation.ApplyDelegate(k, Form.Void);

    }

    public Thunk? Define (Delegate k, Form sym, Form v) {
        var s = sym is Syntax.Identifier i ? i.Symbol : ((Form.Symbol) sym);
        if (_dict.TryAdd(s, v)) return Continuation.ApplyDelegate(k, Form.Void);
        _dict[s] = v;
        return Continuation.ApplyDelegate(k, Form.Void);

    }

    public Thunk? LookUp (Delegate k, Form expr) {
        Syntax.Identifier? id = expr as Syntax.Identifier;
        Form.Symbol symbol = id is not null ? id.Symbol : (Form.Symbol) expr;
        if (_dict.TryGetValue(symbol, out Form? result)) {
            return Continuation.ApplyDelegate(k, result);
        }
        if (Parent is null) {
            throw new Exception($"unbound variable: {symbol.Name} {(id is not null ? id.SrcLoc.ToString() : "")}");
        }
        return Parent.LookUp(k, expr);
    }

    readonly Dictionary<Form.Symbol, Form> _dict = [];

}
