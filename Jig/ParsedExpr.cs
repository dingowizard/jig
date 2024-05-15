using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;

namespace Jig;

public abstract class ParsedExpr : Syntax {

    protected ParsedExpr(Expr x, SrcLoc? srcLoc = null) : base(x, srcLoc) {}

}

public class SetExpr : ParsedExpr {

    private SetExpr(Syntax keyword, Identifier id, Syntax val, SrcLoc? srcLoc = null)
      : base(SyntaxList.FromParams(keyword, id, val), srcLoc) {
        Variable = id;
        Value = val;

    }

    public Identifier Variable {get;}
    public Syntax Value {get;}
    public static bool TryParse(Syntax stx, MacroExpander expander, ExpansionEnvironment ee, [NotNullWhen(returnValue: true)] out SetExpr? setExpr) {
        SyntaxList? stxList = Syntax.E(stx) as SyntaxList;
        if (stxList is null) {
            setExpr = null;
            return false;
        }
        if (!Expr.IsKeyword("set!", stx)) {
            setExpr = null;
            return false;
        }
        Debug.Assert(stxList.Count<Syntax>() == 3); // TODO: this should be a syntax error
        Syntax.Identifier id = stxList.ElementAt<Syntax>(1) as Syntax.Identifier
            ?? throw new Exception($"syntax error: malformed set!: expected first argument to be an identifier. Got {stxList.ElementAt<Syntax>(1)}");
        var x = stxList.ElementAt<Syntax>(2);

        setExpr = new SetExpr(stxList.ElementAt<Syntax>(0),
                              (Syntax.Identifier)expander.Expand(id, ee),
                              expander.Expand(x, ee),
                              stx.SrcLoc);
        return true;
    }

}

public class DefineExpr : ParsedExpr {

    private DefineExpr(Syntax keyword, Identifier id, Syntax val, SrcLoc? srcLoc = null)
      : base(SyntaxList.FromParams(keyword, id, val), srcLoc) {
        Variable = id;
        Value = val;
    }

    public Identifier Variable {get;}
    public Syntax Value {get;}

    public static bool TryParse(Syntax stx, MacroExpander expander, ExpansionEnvironment ee, [NotNullWhen(returnValue: true)] out DefineExpr? defineExpr) {

        SyntaxList? stxList = Syntax.E(stx) as SyntaxList;
        if (stxList is null) {
            defineExpr = null;
            return false;
        }
        if (!Expr.IsKeyword("define", stx)) {
            defineExpr = null;
            return false;
        }
        Debug.Assert(stxList.Count<Syntax>() == 3); // TODO: this should be a syntax error
        // TODO: (define x) is legal too
        Syntax.Identifier id = stxList.ElementAt<Syntax>(1) as Syntax.Identifier
            ?? throw new Exception($"syntax error: malformed define: expected first argument to be an identifier. Got {stxList.ElementAt<Syntax>(1)}");
        if (!expander.Bindings.ContainsKey(id)) {
            id.Symbol.Binding = new Binding();
            expander.Bindings.Add(id, id.Symbol.Binding);
        }
        var x = stxList.ElementAt<Syntax>(2);
        defineExpr = new DefineExpr(stxList.ElementAt<Syntax>(0),
                                    (Syntax.Identifier)expander.Expand(id, ee),
                                    expander.Expand(x, ee),
                                    stx.SrcLoc);
        return true;
    }

}

public class LambdaExpr : ParsedExpr {

    private LambdaExpr(Syntax keyword, Syntax parameters, SyntaxList bodies, SrcLoc? srcLoc = null)
      : base((Expr)Pair.Cons(keyword, (Expr)Pair.Cons(parameters, bodies)), srcLoc)
    {
        Parameters = parameters;
        Bodies = bodies;
    }

    public Syntax Parameters {get;}
    public SyntaxList Bodies {get;}

    public static bool TryParse(Syntax stx, MacroExpander expander, ExpansionEnvironment ee, [NotNullWhen(returnValue: true)] out LambdaExpr? lambdaExpr) {
        SyntaxList? stxList = Syntax.E(stx) as SyntaxList;
        if (stxList is null) {
            lambdaExpr = null;
            return false;
        }
        if (!Expr.IsKeyword("lambda", stx)) {
            lambdaExpr = null;
            return false;
        }
        List<Syntax> xs = new List<Syntax>();
        xs.Add(stxList.ElementAt<Syntax>(0));
        var newScope = new Scope();
        var parameters = stxList.ElementAt<Syntax>(1);
        Syntax.AddScope(parameters, newScope); // TODO: is this necessary? Seems so. deleting it breaks a lot of tests.
        // create a new binding for each parameter
        if (Syntax.E(parameters) is SyntaxList psStxList) {
            foreach(var p in psStxList) {
                Syntax.Identifier id = p as Syntax.Identifier ?? throw new Exception($"ExpandLambda: expected parameters to be identifiers, but got {p}");
                Binding binding = new Binding();
                id.Symbol.Binding = binding;
                expander.Bindings.Add(id, binding);
            }
        } else if (Syntax.E(parameters) is IPair pair) {
            while (pair.Cdr is IPair cdrPair) {
                if (pair.Car is Syntax.Identifier ident) {
                    Binding binding = new Binding();
                    ident.Symbol.Binding = binding;
                    expander.Bindings.Add(ident, binding);
                }
                pair = cdrPair;
            }
            if (pair.Cdr is Syntax.Identifier id) {
                Binding binding = new Binding();
                id.Symbol.Binding = binding;
                expander.Bindings.Add(id, binding);
            }
        } else if (parameters is Syntax.Identifier psId) {
                Binding binding = new Binding();
                psId.Symbol.Binding = binding;
                expander.Bindings.Add(psId, binding);
        } else if (Syntax.E(parameters) is List.NullType) {

        } else {
            throw new Exception($"ExpandLambda: expected parameters to be list or identifier, got {Syntax.E(parameters)}");
        }
        xs.Add(parameters);
        foreach (var x in stxList.Skip<Syntax>(2)) {
            Syntax.AddScope(x, newScope);
            Syntax bodyExpr = expander.Expand(x, ee);
            xs.Add(bodyExpr);
        }
        // TODO: ensure that bodies is non-empty here or in constructor
        // return new Syntax(SyntaxList.FromIEnumerable(xs), srcLoc);
        lambdaExpr = new LambdaExpr(xs.ElementAt(0), xs.ElementAt(1), (SyntaxList)SyntaxList.FromIEnumerable(xs.Skip(2)), stx.SrcLoc);
        return true;
    }
}

public class IfExpr : ParsedExpr {

    public static bool TryParse(Syntax stx, MacroExpander expander, ExpansionEnvironment ee, [NotNullWhen(returnValue: true)] out IfExpr? ifExpr) {
        SyntaxList? stxList = Syntax.E(stx) as SyntaxList;
        if (stxList is null) {
            ifExpr = null;
            return false;
        }
        List<Syntax> xs = new List<Syntax>();
        if (!Expr.IsKeyword("if", stx)) {
            ifExpr = null;
            return false;
        }
        xs.Add(stxList.ElementAt<Syntax>(0));
        foreach (var x in stxList.Skip<Syntax>(1)) {
            Syntax bodyExpr = expander.Expand(x, ee);
            xs.Add(bodyExpr);
        }
        if (xs.Count == 3) {
            ifExpr = new IfExpr(xs.ElementAt(0), xs.ElementAt(1), xs.ElementAt(2), stx.SrcLoc);
            return true;
        } else if (xs.Count == 4) {
            ifExpr = new IfExpr(xs.ElementAt(0), xs.ElementAt(1), xs.ElementAt(2), xs.ElementAt(3), stx.SrcLoc);
            return true;
        } else {
            throw new Exception($"syntax error: malformed 'if' @{stx.SrcLoc}: {stxList}");
        }
    }

    public Syntax Condition {get; private set;}

    public Syntax Then {get; private set;}

    public Syntax? Else {get; private set;}


    private IfExpr(Syntax kywd, Syntax cond, Syntax then, Syntax @else, SrcLoc? srcLoc = null) :
        base(SyntaxList.FromParams(kywd, cond, then, @else), srcLoc)
    {
        Condition = cond;
        Then = then;
        Else = @else;


    }

    private IfExpr(Syntax kywd, Syntax cond, Syntax then, SrcLoc? srcLoc = null) :
        base(SyntaxList.FromParams(kywd, cond, then), srcLoc)
    {
        Condition = cond;
        Then = then;
    }


}
