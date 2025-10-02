using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using Jig.Expansion;

namespace Jig;

public abstract class ParsedForm : Syntax {

    protected ParsedForm(ISchemeValue x, SrcLoc? srcLoc = null) : base(x, srcLoc) {}

}

public class ParsedApplication : ParsedForm {
    public ParsedApplication(IEnumerable<ParsedForm> stxList, SrcLoc? srcLoc) : base(stxList.ToSyntaxList(), srcLoc) {
        ParsedExprs = stxList.ToArray();
    }

    public IEnumerable<ParsedForm> ParsedExprs {get;}
}

public class ParsedSet : ParsedForm {

    internal ParsedSet(Syntax keyword, ParsedVariable id, ParsedForm val, SrcLoc? srcLoc = null)
      : base(SyntaxList.FromParams(keyword, id, val), srcLoc) {
        Variable = id;
        Value = val;
    }

    public ParsedVariable Variable {get;}
    public ParsedForm Value {get;}

}

public class ParsedBegin(Syntax keyword, ParsedForm[] forms, SrcLoc? srcLoc = null) :
    ParsedForm((SchemeValue)Pair.Cons(keyword, SyntaxList.FromIEnumerable(forms)), srcLoc) {
    public ParsedForm[] Forms {get;} = forms;


}

public class ParsedLiteral : ParsedForm {
    internal ParsedLiteral(Syntax keyword, Syntax lit, SrcLoc? srcLoc = null)
    : base (SyntaxList.FromParams(keyword, lit), srcLoc) {
        Quoted = lit;
    }
    
    public new static ParsedLiteral Void => new ParsedLiteral(new Identifier(new Symbol("quote")), new Syntax(SchemeValue.Void));

    public Syntax Quoted {get;}

    public static bool TryParse(Syntax stx, [NotNullWhen(returnValue: true)] out ParsedLiteral? parsedLiteral) {
        if (stx is Literal literal) {
            parsedLiteral = new ParsedLiteral(new Identifier(new Symbol("quote")), literal, stx.SrcLoc);
            return true;
        } else if (Syntax.E(stx) is SchemeValue.VoidType) {
            parsedLiteral = new ParsedLiteral(new Identifier(new Symbol("quote")), new Syntax(SchemeValue.Void), stx.SrcLoc);
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

    private static bool QuoteIsMalformed(SyntaxList stxList) => stxList.Count<Syntax>() != 2;

    private static bool IsQuote(Syntax stx) => SchemeValue.IsKeyword("quote", stx);

}

public class ParsedQuoteSyntax : ParsedForm {
    internal ParsedQuoteSyntax(Syntax keyword, Syntax lit, SrcLoc? srcLoc)
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

    private static bool IsQuote(Syntax stx) => SchemeValue.IsKeyword("quote-syntax", stx);

}

public class ParsedLambda : ParsedForm {

    internal ParsedLambda(Syntax keyword, LambdaParameters parameters, int scopeVarsCount, ParsedForm[] bodies, SrcLoc? srcLoc = null)
      : base(Pair.Cons(keyword, Pair.Cons(parameters, (ISchemeValue)bodies.ToJigList())), srcLoc)
    {
        Parameters = parameters;
        Bodies = bodies;
        ScopeVarsCount = scopeVarsCount;
    }

    public LambdaParameters Parameters {get;}
    
    // number of parameters and local variables declared in body
    public int  ScopeVarsCount {get;}
    
    public ParsedForm[] Bodies {get;}


    public class LambdaParameters : Syntax {
        private LambdaParameters(Syntax stx, ParsedVariable.Lexical[] required, ParsedVariable.Lexical? rest)
            : base(Syntax.E(stx), stx.SrcLoc)
        {
            Required = required;
            Rest = rest;
        }


public static LambdaParameters Parse(Syntax stx, ExpansionContext context) {
            var namesSeen = new System.Collections.Generic.List<string>();
            var required = new System.Collections.Generic.List<ParsedVariable.Lexical>();
            ParsedVariable.Lexical? rest = null;
            if (Syntax.E(stx) is SyntaxList psStxList) {
                foreach(Syntax p in psStxList.Cast<Syntax>()) {
                    Identifier id = p as Identifier ??
                                    throw new Exception($"lambda: expected parameters to be identifiers, but got {p} @ {p.SrcLoc?.ToString() ?? "?"}");
                    if (namesSeen.Contains(id.Symbol.Name)) {
                        throw new Exception($"lambda: expected parameters to have unique names but got {id} more than once @ {id.SrcLoc?.ToString() ?? "?"}");
                    }
                    Parameter parameter = new Parameter(
                        id.Symbol,
                        context.ScopeLevel,
                        context.VarIndex++,
                        id.SrcLoc);
                    id.Symbol.Binding = parameter;
                    context.AddBinding(id, parameter);
                    namesSeen.Add(id.Symbol.Name);
                    required.Add(new ParsedVariable.Lexical(id, parameter, id.SrcLoc));
                }
            } else if (Syntax.E(stx) is IPair pair) {
                Identifier? id = pair.Car as Identifier;
                if (id is null) throw new Exception($"lambda: expected parameters to be identifiers, but got {pair.Car}");
                if (namesSeen.Contains(id.Symbol.Name)) {
                    throw new Exception($"lambda: expected parameters to have unique names but got {id} more than once @ {id.SrcLoc?.ToString() ?? "?"}");
                }
                namesSeen.Add(id.Symbol.Name);
                Parameter parameter =
                    new Parameter(
                        id.Symbol,
                        context.ScopeLevel,
                        context.VarIndex++,
                        id.SrcLoc);
                id.Symbol.Binding = parameter;
                required.Add(new ParsedVariable.Lexical(id, parameter, id.SrcLoc));
                context.AddBinding(id, parameter);
                while (pair.Cdr is IPair cdrPair) {
                    id = cdrPair.Car as Identifier;
                    if (id is null) throw new Exception($"lambda: expected parameters to be identifiers, but got {pair.Car}");
                    if (namesSeen.Contains(id.Symbol.Name)) {
                        throw new Exception($"lambda: expected parameters to have unique names but got {id} more than once @ {id.SrcLoc?.ToString() ?? "?"}");
                    }
                    parameter =
                        new Parameter(
                            id.Symbol,
                            context.ScopeLevel,
                            context.VarIndex++,
                            id.SrcLoc);
                    id.Symbol.Binding = parameter;
                    context.AddBinding(id, parameter);
                    pair = cdrPair;
                    namesSeen.Add(id.Symbol.Name);
                    required.Add(new ParsedVariable.Lexical(id, parameter, id.SrcLoc));
                }
                id = pair.Cdr as Identifier;
                if (id is null) throw new Exception($"lambda: expected parameters to be identifiers, but got {pair.Cdr}");
                if (namesSeen.Contains(id.Symbol.Name)) {
                    throw new Exception($"lambda: expected parameters to have unique names but got {id} more than once @ {id.SrcLoc?.ToString() ?? "?"}");
                }
                parameter = new Parameter(
                    id.Symbol,
                    context.ScopeLevel,
                    context.VarIndex++,
                    id.SrcLoc);
                id.Symbol.Binding = parameter;
                context.AddBinding(id, parameter);
                namesSeen.Add(id.Symbol.Name);
                rest = new ParsedVariable.Lexical(id, parameter, id.SrcLoc);
            } else if (stx is Identifier psId) {
                    Parameter parameter =
                        new Parameter(
                            psId.Symbol,
                            context.ScopeLevel,
                            context.VarIndex++,
                            psId.SrcLoc);
                    psId.Symbol.Binding = parameter;
                    context.AddBinding(psId, parameter);
                    rest = new ParsedVariable.Lexical(psId, parameter, psId.SrcLoc);
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

public class ParsedIf : ParsedForm {

    

    public ParsedForm Condition {get; private set;}

    public ParsedForm Then {get; private set;}

    public ParsedForm? Else {get; private set;}


    internal ParsedIf(Syntax kywd, ParsedForm cond, ParsedForm then, ParsedForm @else, SrcLoc? srcLoc = null) :
        base(SyntaxList.FromParams(kywd, cond, then, @else), srcLoc)
    {
        Condition = cond;
        Then = then;
        Else = @else;


    }

    internal ParsedIf(Syntax kywd, ParsedForm cond, ParsedForm then, SrcLoc? srcLoc = null) :
        base(SyntaxList.FromParams(kywd, cond, then), srcLoc)
    {
        Condition = cond;
        Then = then;
    }


}
