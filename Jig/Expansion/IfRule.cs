namespace Jig.Expansion;

public partial class CoreParseRules {

    public static SemiParsedForm ParseIfForm(Syntax syntax, ExpansionContext context) {

        // TODO: test for number of forms here instead to fail earlier?
        return new SemiParsedIf((SyntaxList)Syntax.E(syntax), syntax.SrcLoc);
        
    }

}

public class SemiParsedIf : SemiParsedExpression {
    public SemiParsedIf(SyntaxList toSyntaxList, SrcLoc? syntaxSrcLoc) : base(toSyntaxList, syntaxSrcLoc) {
        var subForms = toSyntaxList.ToArray<Syntax>();
        int formLength = subForms.Length;
        if (formLength is not (3 or 4)) {
            throw new Exception($"bad syntax in if @ {syntaxSrcLoc}: expected 3 or 4 sub-forms, got {formLength}: {toSyntaxList.Print()}");
        }
        Keyword = (Identifier)subForms[0];
        Condition = subForms[1];
        Then = subForms[2];
        if (formLength == 4) {
            Else = subForms[3];
        }
    }
    public Identifier Keyword {get;}
    public Syntax Condition {get;}
    public Syntax Then {get; set;}

    public Syntax? Else {get;}
    public override ParsedForm Expand(ExpansionContext context) {

        var newContext = context.ExtendWithExpressionContext();
        return Else is not null ?
            new ParsedIf(
                Keyword,
                newContext.Expand(Condition),
                newContext.Expand(Then),
                newContext.Expand(Else),
                SrcLoc) :
            new ParsedIf(
                Keyword,
                newContext.Expand(Condition),
                newContext.Expand(Then),
                SrcLoc);


    }
}
