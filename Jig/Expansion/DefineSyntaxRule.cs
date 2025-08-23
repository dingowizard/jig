namespace Jig.Expansion;

public partial class CoreParseRules {

    public static ParsedForm DefineSyntax(Syntax syntax, ExpansionContext context) {
        var subForms = ((SyntaxList)Syntax.E(syntax)).ToArray<Syntax>();
        int formLength = subForms.Length;
        if (subForms.Length is not 3) {
            throw new Exception($"bad syntax in define-syntax @ {syntax.SrcLoc}: expected 3 sub-forms, got {formLength}");
        }
        ParsedVariable var =
            subForms[1] as ParsedVariable ?? throw new Exception($"bad syntax in define-syntax @ {syntax.SrcLoc}: expected first sub-form to be a parsed variable. Got {subForms[1]}, a {subForms[1].GetType()}");
        // TODO: probably we should only need to check that the syntax is a parsed define syntax at this point
        return new ParsedDefineSyntax(subForms[0], var, subForms[2]);

        // TODO: expand and then evaluate the third sub-form
    }
    
}

