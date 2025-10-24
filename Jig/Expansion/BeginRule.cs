namespace Jig.Expansion;

public partial class CoreParseRules
{
    // TODO: should these all return the more specific type (here, ParsedBegin)?
    public static SemiParsedForm ParseBeginForm(Syntax syntax, ExpansionContext context) {
        var stxList = (SyntaxList)Syntax.E(syntax);
        var subForms = stxList.ToArray<Syntax>();
        if (!context.DefinesAllowed) {
            if (subForms.Length < 2) {
                throw new Exception($"invalid syntax in begin @ {syntax.SrcLoc}: expected at least one expression");
            } 
        }
        var semiParsed = subForms.Skip(1).Select(stx => context.Expander.FirstPass(stx, context));
        return new SemiParsedBegin(semiParsed, stxList, syntax.SrcLoc);
    }

}

public class SemiParsedBegin : SemiParsedForm {
    public SemiParsedBegin(IEnumerable<SemiParsedForm> semiParsed, SyntaxList stxList, SrcLoc? syntaxSrcLoc) : base (stxList, syntaxSrcLoc) {
        Keyword = (Identifier)stxList.ElementAt<Syntax>(0);
        SemiParsed = semiParsed;
    }
    public Identifier Keyword {get; set;}

    public IEnumerable<SemiParsedForm> SemiParsed {get;}

    public override ParsedForm Expand(ExpansionContext context) {
        var fullyParsed = new System.Collections.Generic.List<ParsedForm>();
        foreach (var semiParsed in SemiParsed) {
            // TODO: is this where we decide to change context to definitions not allowed?
            fullyParsed.Add(context.Expander.SecondPass(semiParsed, context));

        }
        return new ParsedBegin(
            Keyword,
            fullyParsed.ToArray<ParsedForm>(),
            SrcLoc);
    }
}