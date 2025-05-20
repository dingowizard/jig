using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;

namespace Jig;

public abstract class ParsedExpr : Syntax {

    protected ParsedExpr(IForm x, SrcLoc? srcLoc = null) : base(x, srcLoc) {}

}

public class ParsedList : ParsedExpr {
    public ParsedList(IEnumerable<ParsedExpr> stxList, SrcLoc? srcLoc) : base(stxList.ToSyntaxList(), srcLoc) {
        ParsedExprs = stxList.ToArray();
    }

    public IEnumerable<ParsedExpr> ParsedExprs {get;}
}

public class ParsedSet : ParsedExpr {

    private ParsedSet(Syntax keyword, ParsedVariable id, ParsedExpr val, SrcLoc? srcLoc = null)
      : base(SyntaxList.FromParams(keyword, id, val), srcLoc) {
        Variable = id;
        Value = val;
    }

    public ParsedVariable Variable {get;}
    public ParsedExpr Value {get;}
    public static bool TryParse(Syntax stx, MacroExpander expander, ExpansionEnvironment ee, [NotNullWhen(returnValue: true)] out ParsedSet? setExpr) {
        if (Syntax.E(stx) is not SyntaxList stxList) {
            setExpr = null;
            return false;
        }
        if (!Form.IsKeyword("set!", stx)) {
            setExpr = null;
            return false;
        }
        Debug.Assert(stxList.Count<Syntax>() == 3); // TODO: this should be a syntax error
        Syntax.Identifier id = stxList.ElementAt<Syntax>(1) as Syntax.Identifier
            ?? throw new Exception($"syntax error: malformed set!: expected first argument to be an identifier. Got {stxList.ElementAt<Syntax>(1)}");
        var x = stxList.ElementAt<Syntax>(2);

        setExpr = new ParsedSet(stxList.ElementAt<Syntax>(0),
                              (ParsedVariable)expander.Expand(id, ee),
                              expander.Expand(x, ee, definesAllowed: false),
                              stx.SrcLoc);
        return true;
    }

}

public class ParsedBegin(Syntax keyword, ParsedExpr[] forms, SrcLoc? srcLoc = null) :
ParsedExpr((Form)Pair.Cons(keyword, SyntaxList.FromIEnumerable(forms)), srcLoc) {
    public ParsedExpr[] Forms {get;} = forms;

    public static bool TryParse(Syntax stx,
                                MacroExpander expander,
                                ExpansionEnvironment ee,
                                bool definesAllowed,
                                [NotNullWhen(returnValue: true)] out ParsedBegin? beginExpr)
    {
        if (Syntax.E(stx) is not SyntaxList.NonEmpty stxList)
        {
            beginExpr = null;
            return false;
        }
        if (!Form.IsKeyword("begin", stx)) {
            beginExpr = null;
            return false;
        }
        System.Collections.Generic.List<ParsedExpr> forms = [];
        foreach (var x in stxList.Skip<Syntax>(1)) {
            forms.Add(expander.Expand(x, ee, definesAllowed));
        }
        beginExpr = new ParsedBegin(stxList.ElementAt<Syntax>(0), forms.ToArray(), stx.SrcLoc);
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
        } else if (Syntax.E(stx) is Form.VoidType) {
            parsedLiteral = new ParsedLiteral(new Syntax.Identifier(new Symbol("quote")), new Syntax(Form.Void), stx.SrcLoc);
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

    private static bool IsQuote(Syntax stx) => Form.IsKeyword("quote", stx);

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

    private static bool IsQuote(Syntax stx) => Form.IsKeyword("quote-syntax", stx);

}

public class ParsedDefine : ParsedExpr {

    private ParsedDefine(Syntax keyword, ParsedVariable id, ParsedExpr val, SrcLoc? srcLoc = null)
      : base(SyntaxList.FromParams(keyword, id, val), srcLoc) {
        Variable = id;
        Value = val;
    }

    private ParsedDefine(Syntax keyword, ParsedVariable id, SrcLoc? srcLoc = null)
        : base(SyntaxList.FromParams(keyword, id), srcLoc) {
        Variable = id;
        Value = null;
    }
    public ParsedVariable Variable {get;}
    public ParsedExpr? Value {get;}

    public static bool TryParse(Syntax stx, MacroExpander expander, ExpansionEnvironment ee, [NotNullWhen(returnValue: true)] out ParsedDefine? defineExpr) {
        if (Syntax.E(stx) is not SyntaxList stxList) {
            defineExpr = null;
            return false;
        }
        if (!Form.IsKeyword("define", stx)) {
            defineExpr = null;
            return false;
        }
        Debug.Assert(stxList.Count<Syntax>() == 3 || stxList.Count<Syntax>() == 2); // TODO: this should be a syntax error
        Syntax.Identifier id = stxList.ElementAt<Syntax>(1) as Syntax.Identifier
            ?? throw new Exception($"syntax error: malformed define: expected first argument to be an identifier. Got {stxList.ElementAt<Syntax>(1)}");
        // TODO: hm...
        if (id.ScopeSet.Count != 0) {
            // not top level
            var binding = new Binding(id.Symbol,  ee.ScopeLevel, ee.VarIndex++);
            id.Symbol.Binding = binding ;
            expander.AddBinding(id, id.Symbol.Binding);
            defineExpr = new ParsedDefine(stxList.ElementAt<Syntax>(0),
                                        new ParsedVariable.Lexical(id, binding, id.SrcLoc),
                                        expander.Expand(stxList.ElementAt<Syntax>(2), ee, definesAllowed: false),
                                        stx.SrcLoc);
            return true;
        }

        defineExpr = stxList.Count<Syntax>() == 3
            ? new ParsedDefine(stxList.ElementAt<Syntax>(0),
                new ParsedVariable.TopLevel(id, id.SrcLoc),
                expander.Expand(stxList.ElementAt<Syntax>(2), ee, definesAllowed: false),
                stx.SrcLoc)
            : new ParsedDefine(stxList.ElementAt<Syntax>(0),
                new ParsedVariable.TopLevel(id, id.SrcLoc), stx.SrcLoc);
            
        return true;
    }

}

public class ParsedLambda : ParsedExpr {

    private ParsedLambda(Syntax keyword, LambdaParameters parameters, int scopeVarsCount, SyntaxList.NonEmpty bodies, SrcLoc? srcLoc = null)
      : base(Pair.Cons(keyword, Pair.Cons(parameters, bodies)), srcLoc)
    {
        Parameters = parameters;
        Bodies = bodies;
        ScopeVarsCount = scopeVarsCount;
    }

    public LambdaParameters Parameters {get;}
    
    // number of parameters and local variables declared in body
    public int  ScopeVarsCount {get;}
    
    public SyntaxList.NonEmpty Bodies {get;}

    public static bool TryParse(Syntax stx,
                                MacroExpander expander,
                                ExpansionEnvironment ee,
                                [NotNullWhen(returnValue: true)] out ParsedLambda? lambdaExpr)
    {
        if (Syntax.E(stx) is not SyntaxList stxList)
        {
            lambdaExpr = null;
            return false;
        }
        if (!Form.IsKeyword("lambda", stx)) {
            lambdaExpr = null;
            return false;
        }
        System.Collections.Generic.List<Syntax> xs = [
            stxList.ElementAt<Syntax>(0)
        ];
        var newScope = new Scope();
        var parameters = stxList.ElementAt<Syntax>(1);
        Syntax.AddScope(parameters, newScope);
        ee = ee.Extend();
        
        LambdaParameters ps = LambdaParameters.Parse(parameters, expander, ee);

        xs.Add(parameters);
        foreach (var x in stxList.Skip<Syntax>(2)) {
            Syntax.AddScope(x, newScope);
            Syntax bodyExpr = expander.Expand(x, ee);
            xs.Add(bodyExpr);
        }

        // int numVars = ee.VarIndex == 0 ? ps.Required.Length + (ps.HasRequired ? 1 : 0) : ee.VarIndex + 1;
        // TODO: ensure that bodies is non-empty here or in constructor
        lambdaExpr = new ParsedLambda(xs.ElementAt(0), ps, ee.VarIndex, (SyntaxList.NonEmpty)xs.Skip(2).ToSyntaxList(), stx.SrcLoc);
        return true;
    }

    public class LambdaParameters : Syntax {
        private LambdaParameters(Syntax stx, ParsedVariable.Lexical[] required, ParsedVariable.Lexical? rest)
            : base(Syntax.E(stx), stx.SrcLoc)
        {
            Required = required;
            Rest = rest;
        }

        public static LambdaParameters Parse(Syntax stx, MacroExpander expander, ExpansionEnvironment ee) {
            var namesSeen = new System.Collections.Generic.List<string>();
            var required = new System.Collections.Generic.List<ParsedVariable.Lexical>();
            ParsedVariable.Lexical? rest = null;
            if (Syntax.E(stx) is SyntaxList psStxList) {
                foreach(Syntax p in psStxList.Cast<Syntax>()) {
                    Syntax.Identifier id = p as Syntax.Identifier ??
                        throw new Exception($"lambda: expected parameters to be identifiers, but got {p} @ {p.SrcLoc?.ToString() ?? "?"}");
                    if (namesSeen.Contains(id.Symbol.Name)) {
                        throw new Exception($"lambda: expected parameters to have unique names but got {id} more than once @ {id.SrcLoc?.ToString() ?? "?"}");
                    }
                    Binding binding = new Binding(id.Symbol, ee.ScopeLevel, ee.VarIndex++);
                    id.Symbol.Binding = binding;
                    expander.AddBinding(id, binding);
                    namesSeen.Add(id.Symbol.Name);
                    required.Add(new ParsedVariable.Lexical(id, binding, id.SrcLoc));
                }
            } else if (Syntax.E(stx) is IPair pair) {
                Syntax.Identifier? id = pair.Car as Syntax.Identifier;
                if (id is null) throw new Exception($"lambda: expected parameters to be identifiers, but got {pair.Car}");
                if (namesSeen.Contains(id.Symbol.Name)) {
                    throw new Exception($"lambda: expected parameters to have unique names but got {id} more than once @ {id.SrcLoc?.ToString() ?? "?"}");
                }
                namesSeen.Add(id.Symbol.Name);
                Binding binding = new Binding(id.Symbol, ee.ScopeLevel, ee.VarIndex++);
                id.Symbol.Binding = binding;
                required.Add(new ParsedVariable.Lexical(id, binding, id.SrcLoc));
                expander.AddBinding(id, binding);
                while (pair.Cdr is IPair cdrPair) {
                    id = cdrPair.Car as Syntax.Identifier;
                    if (id is null) throw new Exception($"lambda: expected parameters to be identifiers, but got {pair.Car}");
                    if (namesSeen.Contains(id.Symbol.Name)) {
                        throw new Exception($"lambda: expected parameters to have unique names but got {id} more than once @ {id.SrcLoc?.ToString() ?? "?"}");
                    }
                    binding = new Binding(id.Symbol, ee.ScopeLevel, ee.VarIndex++);
                    id.Symbol.Binding = binding;
                    expander.AddBinding(id, binding);
                    pair = cdrPair;
                    namesSeen.Add(id.Symbol.Name);
                    required.Add(new ParsedVariable.Lexical(id, binding, id.SrcLoc));
                }
                id = pair.Cdr as Syntax.Identifier;
                if (id is null) throw new Exception($"lambda: expected parameters to be identifiers, but got {pair.Cdr}");
                if (namesSeen.Contains(id.Symbol.Name)) {
                    throw new Exception($"lambda: expected parameters to have unique names but got {id} more than once @ {id.SrcLoc?.ToString() ?? "?"}");
                }
                binding = new Binding(id.Symbol, ee.ScopeLevel, ee.VarIndex++);
                id.Symbol.Binding = binding;
                expander.AddBinding(id, binding);
                namesSeen.Add(id.Symbol.Name);
                rest = new ParsedVariable.Lexical(id, binding, id.SrcLoc);
            } else if (stx is Syntax.Identifier psId) {
                    Binding binding = new Binding(psId.Symbol, ee.ScopeLevel, ee.VarIndex++);
                    psId.Symbol.Binding = binding;
                    expander.AddBinding(psId, binding);
                    rest = new ParsedVariable.Lexical(psId, binding, psId.SrcLoc);
            } else if (Syntax.E(stx) is List.Empty) {

            } else {
                throw new Exception($"ExpandLambda: expected parameters to be list or identifier, got {Syntax.E(stx)}");
            }
            return new LambdaParameters(stx, required.ToArray(), rest);
        }

        public bool HasRequired => Required.Length != 0;

        public bool HasRest => Rest is not null;

        public ParsedVariable.Lexical[] Required { get; }
        public ParsedVariable.Lexical? Rest { get; }
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
        // TODO: make binding a required field for parsed variables
        if (stx is Syntax.Identifier id) {
            if(expander.TryResolve(id, out var binding)) {
                // if (id.Symbol.Name == "y") {
                //     Console.WriteLine($"found binding for y: {binding}");
                // }
                id.Symbol.Binding = binding;
                parsedVariable = new ParsedVariable.Lexical(id, binding, stx.SrcLoc);
                return true;
            } else {
                // if (id.Symbol.Name == "y") {
                //     Console.WriteLine("couldn't resolve y");
                // }
                
                // TODO: toplevel vars still _do_ have a binding, right? a module binding?
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
        internal Lexical(Syntax.Identifier id, Binding binding, SrcLoc? srcLoc) : base(id, srcLoc) {
            Binding = binding;
        }
        
        public Binding Binding { get; }

    }

    public Identifier Identifier { get; }
}

public class ParsedIf : ParsedExpr {

    public static bool TryParse(Syntax stx, MacroExpander expander, ExpansionEnvironment ee, [NotNullWhen(returnValue: true)] out ParsedIf? ifExpr) {
        if (Syntax.E(stx) is not SyntaxList stxList)
        {
            ifExpr = null;
            return false;
        }
        System.Collections.Generic.List<ParsedExpr> xs = [];
        if (!Form.IsKeyword("if", stx)) {
            ifExpr = null;
            return false;
        }
        foreach (var x in stxList.Skip<Syntax>(1)) {
            ParsedExpr bodyExpr = expander.Expand(x, ee, definesAllowed: false);
            xs.Add(bodyExpr);
        }
        if (xs.Count == 2) {
            ifExpr = new ParsedIf(stxList.ElementAt<Syntax>(0), xs.ElementAt(0), xs.ElementAt(1), stx.SrcLoc);
            return true;
        } else if (xs.Count == 3) {
            ifExpr = new ParsedIf(stxList.ElementAt<Syntax>(0), xs.ElementAt(0), xs.ElementAt(1), xs.ElementAt(2), stx.SrcLoc);
            return true;
        } else {
            throw new Exception($"syntax error: malformed 'if' @{stx.SrcLoc}: {stxList}");
        }
    }

    public ParsedExpr Condition {get; private set;}

    public ParsedExpr Then {get; private set;}

    public ParsedExpr? Else {get; private set;}


    private ParsedIf(Syntax kywd, ParsedExpr cond, ParsedExpr then, ParsedExpr @else, SrcLoc? srcLoc = null) :
        base(SyntaxList.FromParams(kywd, cond, then, @else), srcLoc)
    {
        Condition = cond;
        Then = then;
        Else = @else;


    }

    private ParsedIf(Syntax kywd, ParsedExpr cond, ParsedExpr then, SrcLoc? srcLoc = null) :
        base(SyntaxList.FromParams(kywd, cond, then), srcLoc)
    {
        Condition = cond;
        Then = then;
    }


}
