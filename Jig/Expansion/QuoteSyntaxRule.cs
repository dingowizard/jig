namespace Jig.Expansion;

public partial class CoreParseRules{ 
    public static SemiParsedForm ParseQuoteSyntaxForm(Syntax syntax, ExpansionContext context) {
        // TODO: really not sure how quote-syntax is supposed to work
        return new SemiParsedQuoteSyntax((SyntaxList)Syntax.E(syntax), syntax.SrcLoc);

    }

}

public class SemiParsedQuoteSyntax : SemiParsedExpression {
    public SemiParsedQuoteSyntax(SyntaxList stxList, SrcLoc? srcLoc) : base(stxList, srcLoc) {
        SubForms = stxList.ToArray<Syntax>();
        if (SubForms.Length != 2) {
            throw new Exception($"bad syntax in quote-syntax @ {SrcLoc}: expected 2 sub-forms");
        }
        
    }
    public Syntax[] SubForms {get; set;}

    public override ParsedForm SecondPass(ExpansionContext context) {
        return new ParsedQuoteSyntax(SubForms[0], SubForms[1], SrcLoc);
    }
}
