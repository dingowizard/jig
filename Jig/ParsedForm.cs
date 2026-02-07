using System.Diagnostics.CodeAnalysis;

namespace Jig;

public abstract class ParsedForm : Syntax {

    protected ParsedForm(SchemeValue x, SrcLoc? srcLoc = null) : base(x, srcLoc) {}

}

public abstract class Definition : ParsedForm {
    protected Definition(SchemeValue x, SrcLoc? srcLoc = null) : base(x, srcLoc) {}
}

public abstract class Expression : ParsedForm {
    protected Expression(SchemeValue x, SrcLoc? srcLoc = null) : base(x, srcLoc) {}
}
public class ParsedApplication : Expression {
    public ParsedApplication(IEnumerable<ParsedForm> stxList, SrcLoc? srcLoc) : base(stxList.ToSyntaxList(), srcLoc) {
        ParsedExprs = stxList.ToArray();
    }

    public IEnumerable<ParsedForm> ParsedExprs {get;}
}

public class ParsedSet : Expression {

    internal ParsedSet(Syntax keyword, ParsedVariable id, ParsedForm val, SrcLoc? srcLoc = null)
      : base(SyntaxList.FromParams(keyword, id, val), srcLoc) {
        Variable = id;
        Value = val;
    }

    public ParsedVariable Variable {get;}
    public ParsedForm Value {get;}

}

public class ParsedBegin(Syntax keyword, ParsedForm[] forms, SrcLoc? srcLoc = null) :
    // TODO: there are begin forms that are definitions and begin forms that are expressions
    ParsedForm((SchemeValue)Pair.Cons(keyword, SyntaxList.FromIEnumerable(forms)), srcLoc) {
    public ParsedForm[] Forms {get;} = forms;

}

public class ParsedLiteral : Expression {
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

public class ParsedQuoteSyntax : Expression {
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

public class ParsedIf : Expression {
    // should we subclass with ParsedIfThen and ParsedIfThenElse?

    

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
