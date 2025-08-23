namespace Jig.Expansion;

public partial class CoreParseRules{ 
    public static ParsedForm ParseQuoteSyntaxForm(Syntax syntax, ExpansionContext context) {

        var subForms = ((SyntaxList)Syntax.E(syntax)).ToArray<Syntax>();
        if (subForms.Length != 2) {
            throw new Exception($"bad syntax in quote-syntax @ {syntax.SrcLoc}: expected 2 sub-forms");
        }
        return new ParsedQuoteSyntax(subForms[0], subForms[1], syntax.SrcLoc);
    }

}
