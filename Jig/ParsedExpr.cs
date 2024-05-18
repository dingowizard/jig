using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;

namespace Jig;

public abstract class ParsedExpr : Syntax {

    protected ParsedExpr(Expr x, SrcLoc? srcLoc = null) : base(x, srcLoc) {}

}

public class ParsedList : ParsedExpr {
    public ParsedList(SyntaxList stxList, SrcLoc? srcLoc) : base(stxList, srcLoc) {
        ParsedExprs = stxList.Cast<ParsedExpr>().ToArray();
    }

    public IEnumerable<ParsedExpr> ParsedExprs {get;}
}

public class ParsedSet : ParsedExpr {

    private ParsedSet(Syntax keyword, ParsedVariable id, Syntax val, SrcLoc? srcLoc = null)
      : base(SyntaxList.FromParams(keyword, id, val), srcLoc) {
        Variable = id;
        Value = val;

    }

    public ParsedVariable Variable {get;}
    public Syntax Value {get;}
    public static bool TryParse(Syntax stx, MacroExpander expander, ExpansionEnvironment ee, [NotNullWhen(returnValue: true)] out ParsedSet? setExpr) {
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

        setExpr = new ParsedSet(stxList.ElementAt<Syntax>(0),
                              (ParsedVariable)expander.Expand(id, ee),
                              expander.Expand(x, ee),
                              stx.SrcLoc);
        return true;
    }

}

public class ParsedLiteral : ParsedExpr {

    private ParsedLiteral(Syntax keyword, Syntax lit, SrcLoc? srcLoc)
    : base (SyntaxList.FromParams(keyword, lit), srcLoc) {
        Quoted = lit;
    }

    public Syntax Quoted {get;}

    public static bool TryParse(Syntax stx, [NotNullWhen(returnValue: true)] out ParsedLiteral? parsedLiteral) {
        if (stx is Literal literal) {
            parsedLiteral = new ParsedLiteral(new Syntax.Identifier(new Symbol("quote")), literal, stx.SrcLoc);
            return true;
        } else if (IsQuote(stx)) {
            SyntaxList stxList = (SyntaxList)Syntax.E(stx);
            if (QuoteIsMalformed(stxList)) throw new Exception($"malformed quote expression: {stx} @ {stx.SrcLoc}");
            parsedLiteral = new ParsedLiteral(stxList.ElementAt<Syntax>(0), stxList.ElementAt<Syntax>(1), stx.SrcLoc);
            return true;
        }
        parsedLiteral = null;
        return false;
    }

    private static bool QuoteIsMalformed(SyntaxList stxList) => !(stxList.Count<Syntax>() == 2);

    private static bool IsQuote(Syntax stx) => Expr.IsKeyword("quote", stx);

}

public class ParsedQuoteSyntax : ParsedExpr {
    private ParsedQuoteSyntax(Syntax keyword, Syntax lit, SrcLoc? srcLoc)
    : base (SyntaxList.FromParams(keyword, lit), srcLoc) {
        Quoted = lit;
    }

    public Syntax Quoted {get;}

    public static bool TryParse(Syntax stx, [NotNullWhen(returnValue: true)] out ParsedQuoteSyntax? parsedLiteral) {
        if (IsQuote(stx)) {
            SyntaxList stxList = (SyntaxList)Syntax.E(stx);
            if (QuoteIsMalformed(stxList)) throw new Exception($"malformed quote-sytax expression: {stx} @ {stx.SrcLoc}");
            parsedLiteral = new ParsedQuoteSyntax(stxList.ElementAt<Syntax>(0), stxList.ElementAt<Syntax>(1), stx.SrcLoc);
            return true;
        }
        parsedLiteral = null;
        return false;
    }

    private static bool QuoteIsMalformed(SyntaxList stxList) => !(stxList.Count<Syntax>() == 2);

    private static bool IsQuote(Syntax stx) => Expr.IsKeyword("quote-syntax", stx);

}

public class ParsedDefine : ParsedExpr {

    private ParsedDefine(Syntax keyword, ParsedVariable id, Syntax val, SrcLoc? srcLoc = null)
      : base(SyntaxList.FromParams(keyword, id, val), srcLoc) {
        Variable = id;
        Value = val;
    }

    public ParsedVariable Variable {get;}
    public Syntax Value {get;}

    public static bool TryParse(Syntax stx, MacroExpander expander, ExpansionEnvironment ee, [NotNullWhen(returnValue: true)] out ParsedDefine? defineExpr) {

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
        // Console.WriteLine($"parsing define {stx}: {id} has scope set {string.Join(", ", id.ScopeSet)}");
        if (!expander.Bindings.ContainsKey(id)) {
            if (id.ScopeSet.Count != 0) {
                id.Symbol.Binding = new Binding();
                expander.Bindings.Add(id, id.Symbol.Binding);
            }
        }
        var x = stxList.ElementAt<Syntax>(2);
        defineExpr = new ParsedDefine(stxList.ElementAt<Syntax>(0),
                                    (ParsedVariable)expander.Expand(id, ee),
                                    expander.Expand(x, ee),
                                    stx.SrcLoc);
        return true;
    }

}

public class ParsedLambda : ParsedExpr {

    private ParsedLambda(Syntax keyword, Syntax parameters, SyntaxList bodies, SrcLoc? srcLoc = null)
      : base((Expr)Pair.Cons(keyword, (Expr)Pair.Cons(parameters, bodies)), srcLoc)
    {
        Parameters = parameters;
        Bodies = bodies;
    }

    public Syntax Parameters {get;}
    public SyntaxList Bodies {get;}

    public static bool TryParse(Syntax stx, MacroExpander expander, ExpansionEnvironment ee, [NotNullWhen(returnValue: true)] out ParsedLambda? lambdaExpr) {
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
            Syntax.Identifier? id = pair.Car as Syntax.Identifier;
            if (id is null) throw new Exception("lambda: expected all parameters to be identifiers");
            Binding binding = new Binding();
            id.Symbol.Binding = binding;
            expander.Bindings.Add(id, binding);
            while (pair.Cdr is IPair cdrPair) {
                id = cdrPair.Car as Syntax.Identifier;
                if (id is null) throw new Exception("lambda: expected all parameters to be identifiers");
                binding = new Binding();
                id.Symbol.Binding = binding;
                expander.Bindings.Add(id, binding);
                pair = cdrPair;
            }
            id = pair.Cdr as Syntax.Identifier;
            if (id is null) throw new Exception("lambda: expected all parameters to be identifiers");
            binding = new Binding();
            id.Symbol.Binding = binding;
            expander.Bindings.Add(id, binding);
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
        lambdaExpr = new ParsedLambda(xs.ElementAt(0), xs.ElementAt(1), (SyntaxList)SyntaxList.FromIEnumerable(xs.Skip(2)), stx.SrcLoc);
        return true;
    }
}

public class ParsedVariable : ParsedExpr {

    public static bool TryParse(Syntax stx, MacroExpander expander, [NotNullWhen(returnValue: true)] out ParsedVariable? parsedVariable) {
        // if (stx is ParsedVariable pvar) {
        //     if(expander.TryResolve(pvar.Identifier, out Binding? binding)) {
        //         pvar.Identifier.Symbol.Binding = binding;
        //         parsedVariable = new ParsedVariable.Lexical(pvar.Identifier, stx.SrcLoc);
        //         return true;
        //     } else {
        //         parsedVariable = pvar;
        //         return true;
        //     }

        // }
        if (stx is Syntax.Identifier id) {
            if(expander.TryResolve(id, out Binding? binding)) {
                id.Symbol.Binding = binding;
                parsedVariable = new ParsedVariable.Lexical(id, stx.SrcLoc);
                return true;
            } else {
                parsedVariable = new ParsedVariable.TopLevel(id, stx.SrcLoc);
                return true;
            }

        } else {
            parsedVariable = null;
            return false;
        }
    }

    private ParsedVariable(Syntax.Identifier id, SrcLoc? srcLoc) : base (id.Symbol, srcLoc) {
        Identifier = id;
    }

    public class TopLevel : ParsedVariable {
        internal TopLevel(Syntax.Identifier id, SrcLoc? srcLoc) : base (id, srcLoc) {}
    }

    public class Lexical : ParsedVariable {
        internal Lexical(Syntax.Identifier id, SrcLoc? srcLoc) : base (id, srcLoc) {}

    }

    public new Identifier Identifier { get; }
}

public class ParsedIf : ParsedExpr {

    public static bool TryParse(Syntax stx, MacroExpander expander, ExpansionEnvironment ee, [NotNullWhen(returnValue: true)] out ParsedIf? ifExpr) {
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
            ifExpr = new ParsedIf(xs.ElementAt(0), xs.ElementAt(1), xs.ElementAt(2), stx.SrcLoc);
            return true;
        } else if (xs.Count == 4) {
            ifExpr = new ParsedIf(xs.ElementAt(0), xs.ElementAt(1), xs.ElementAt(2), xs.ElementAt(3), stx.SrcLoc);
            return true;
        } else {
            throw new Exception($"syntax error: malformed 'if' @{stx.SrcLoc}: {stxList}");
        }
    }

    public Syntax Condition {get; private set;}

    public Syntax Then {get; private set;}

    public Syntax? Else {get; private set;}


    private ParsedIf(Syntax kywd, Syntax cond, Syntax then, Syntax @else, SrcLoc? srcLoc = null) :
        base(SyntaxList.FromParams(kywd, cond, then, @else), srcLoc)
    {
        Condition = cond;
        Then = then;
        Else = @else;


    }

    private ParsedIf(Syntax kywd, Syntax cond, Syntax then, SrcLoc? srcLoc = null) :
        base(SyntaxList.FromParams(kywd, cond, then), srcLoc)
    {
        Condition = cond;
        Then = then;
    }


}
