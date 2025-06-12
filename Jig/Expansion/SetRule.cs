namespace Jig.Expansion;

public partial class CoreParseRules {
    public static ParsedExpr ParseSetForm(Syntax syntax, ExpansionContext context) {
        
        var subForms = ((SyntaxList)Syntax.E(syntax)).ToArray<Syntax>();
        if (subForms.Length != 3) {
            throw new Exception($"bad syntax in set! @ {syntax.SrcLoc}: expected 3 sub-forms");
        }
        Syntax.Identifier id = subForms[1] as Syntax.Identifier
            ?? throw new Exception($"bad syntax in set! @ {syntax.SrcLoc}: expected first sub-form to be an identifier. Got {subForms[1]}");
        var x = subForms[2];

        return new ParsedSet(subForms.ElementAt<Syntax>(0),
            (ParsedVariable)context.Expand(id),
            context.Expand(x),
            syntax.SrcLoc);
    }

}