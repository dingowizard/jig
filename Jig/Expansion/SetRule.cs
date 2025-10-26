namespace Jig.Expansion;

public partial class CoreParseRules {
    public static SemiParsedForm ParseSetForm(Syntax syntax, ExpansionContext context) {
        
        return new SemiParsedSet((SyntaxList)Syntax.E(syntax), syntax.SrcLoc);

    }

}

public class SemiParsedSet : SemiParsedExpression {
    public SemiParsedSet(SyntaxList stxList, SrcLoc? srcLoc) : base(stxList, srcLoc) {
        SubForms = stxList.ToArray<Syntax>();
        if (SubForms.Length != 3) {
            throw new Exception($"bad syntax in set! @ {SrcLoc}: expected 3 sub-forms");
        }
        
    }
    public Syntax[] SubForms {get;}

    public override ParsedForm SecondPass(ExpansionContext context) {
        Identifier id = SubForms[1] as Identifier
                        ?? throw new Exception($"bad syntax in set! @ {SrcLoc}: expected first sub-form to be an identifier. Got {SubForms[1]}");
        var x = SubForms[2];
        
        return new ParsedSet(SubForms[0],
            (ParsedVariable)context.Expand(id),
            context.Expand(x),
            SrcLoc);
    }
}
