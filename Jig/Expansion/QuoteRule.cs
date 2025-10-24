namespace Jig.Expansion;

public partial class CoreParseRules {
    public static SemiParsedForm ParseQuoteForm(Syntax syntax, ExpansionContext context) {
        return new SemiParsedQuote((SyntaxList)Syntax.E(syntax), syntax.SrcLoc);
        
    }
    
}

public class SemiParsedQuote : SemiParsedExpression {

    public SemiParsedQuote(SyntaxList stxList, SrcLoc? srcLoc = null) : base(stxList, srcLoc) {
        SubForms = stxList.ToArray<Syntax>();
        if (SubForms.Length != 2) throw new Exception($"malformed quote expression: {stxList.Print()} @ {srcLoc}");
    }
    public Syntax[] SubForms {get;}

    public override ParsedForm Expand(ExpansionContext context) {
        return new ParsedLiteral(SubForms[0], SubForms[1], SrcLoc);
    }
}