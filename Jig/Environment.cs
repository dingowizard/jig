namespace Jig;

public class Environment : IEnvironment {

    public Environment() {
        _dict.Add(new Form.Symbol("car"), new Procedure((Builtin) Builtins.car));
        _dict.Add(new Form.Symbol("cdr"), new Procedure((Builtin) Builtins.cdr));
        _dict.Add(new Form.Symbol("cons"), new Procedure((Builtin) Builtins.cons));
        _dict.Add(new Form.Symbol("null?"), new Procedure((Builtin) Builtins.nullP));
        _dict.Add(new Form.Symbol("succ"), new Procedure((Builtin) Builtins.succ));
        _dict.Add(new Form.Symbol("number?"), new Procedure((Builtin) Builtins.number_p));
        _dict.Add(new Form.Symbol("procedure?"), new Procedure((Builtin) Procedure.procedure_p));
        _dict.Add(new Form.Symbol("+"), new Procedure((Builtin) Builtins.sum));
        _dict.Add(new Form.Symbol("*"), new Procedure((Builtin) Builtins.new_product));
        _dict.Add(new Form.Symbol("-"), new Procedure((PairFunction) Builtins.diff));
        _dict.Add(new Form.Symbol("="), new Procedure((PairFunction) Builtins.numEq));
        _dict.Add(new Form.Symbol(">"), new Procedure( (Builtin)Builtins.gt));
        _dict.Add(new Form.Symbol("apply"), new Procedure( Builtins.apply));
        _dict.Add(new Form.Symbol("call/cc"), new Procedure( (Builtin)Builtins.callcc));
        _dict.Add(new Form.Symbol("call-with-current-continuation"), new Procedure( (Builtin)Builtins.callcc));
        _dict.Add(new Form.Symbol("call-with-values"), new Procedure( Continuation.call_with_values));
        _dict.Add(new Form.Symbol("values"), new Procedure( (Builtin)Builtins.values));
        _dict.Add(new Form.Symbol("syntax->list"), new Procedure( (Builtin)Builtins.syntax_to_list));
        _dict.Add(new Form.Symbol("syntax?"), new Procedure( (Builtin)Builtins.syntax_p));
        _dict.Add(new Form.Symbol("datum->syntax"), new Procedure( (Builtin)Builtins.datum_to_syntax));
        _dict.Add(new Form.Symbol("syntax->datum"), new Procedure( (Builtin)Builtins.syntax_to_datum));
        _dict.Add(new Form.Symbol("expand"), new Procedure( (Builtin)Builtins.expand));
        _dict.Add(new Form.Symbol("expand-once"), new Procedure( (Builtin)Builtins.expand_once));
        _dict.Add(new Form.Symbol("pair?"), new Procedure( (Builtin)Builtins.pair_p));
        _dict.Add(new Form.Symbol("symbol=?"), new Procedure( (Builtin)Builtins.symbol_equal_p));
        _dict.Add(new Form.Symbol("symbol?"), new Procedure( (Builtin)Builtins.symbol_p));
        _dict.Add(new Form.Symbol("symbol->string"), new Procedure( (Builtin)Builtins.symbol_to_string));
        _dict.Add(new Form.Symbol("string->symbol"), new Procedure( (Builtin)Builtins.string_to_symbol));
        _dict.Add(new Form.Symbol("syntax-e"), new Procedure( (Builtin)Builtins.syntax_e));
        _dict.Add(new Form.Symbol("char?"), new Procedure( (Builtin)Builtins.char_p));
        _dict.Add(new Form.Symbol("eq?"), new Procedure( (Builtin)Builtins.eq_p));
        _dict.Add(new Form.Symbol("eqv?"), new Procedure( (Builtin)Builtins.eqv_p));
        _dict.Add(new Form.Symbol("display"), new Procedure( (Builtin)Builtins.display));
        _dict.Add(new Form.Symbol("newline"), new Procedure( (Builtin)Builtins.newline));
        _dict.Add(new Form.Symbol("error"), new Procedure( (Builtin)Builtins.error));
        _dict.Add(new Form.Symbol("vector"), new Procedure( (Builtin)Builtins.vector));
        _dict.Add(new Form.Symbol("vector?"), new Procedure( (Builtin)Builtins.vector_p));
        _dict.Add(new Form.Symbol("vector-length"), new Procedure( (Builtin)Builtins.vector_length));
        _dict.Add(new Form.Symbol("vector-ref"), new Procedure( (Builtin)Builtins.vector_ref));
        _dict.Add(new Form.Symbol("append"), new Procedure( (Builtin)Builtins.append));
        _dict.Add(new Form.Symbol("make-record-type-descriptor"), new Procedure( (Builtin)Builtins.make_record_type_descriptor));
        _dict.Add(new Form.Symbol("record-type-descriptor?"), new Procedure( (Builtin)Builtins.record_type_descriptor_p));
        _dict.Add(new Form.Symbol("record-constructor-descriptor?"), new Procedure( (Builtin)Builtins.record_constructor_descriptor_p));
        _dict.Add(new Form.Symbol("make-record-constructor-descriptor"), new Procedure( (Builtin)Builtins.make_record_constructor_descriptor));
        _dict.Add(new Form.Symbol("record?"), new Procedure( (Builtin)Builtins.record_p));
        _dict.Add(new Form.Symbol("record-predicate"), new Procedure( (Builtin)Builtins.record_predicate));
        _dict.Add(new Form.Symbol("record-accessor"), new Procedure( (Builtin)Builtins.record_accessor));
        _dict.Add(new Form.Symbol("record-constructor"), new Procedure( (Builtin)Builtins.record_constructor));
        _dict.Add(new Form.Symbol("map"), new Procedure( (Builtin)Builtins.map));
        // _dict.Add(new Expr.Symbol("dynamic-wind"), new Procedure( (Builtin)Builtins.dynamic_wind));
        // _dict.Add(new Expr.Symbol("error"), new Procedure( (Builtin)Builtins.error));
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
        } else {
            throw new Exception($"unbound variable: {symbol.Name} {(id is not null ? id.SrcLoc.ToString() : "")}");
        }
    }

    readonly Dictionary<Form.Symbol, Form> _dict = [];

}
