namespace Jig.Expansion;

public partial class CoreParseRules {

    public static ParsedForm ParseDefineForm(Syntax syntax, ExpansionContext context) {

        var subForms = ((SyntaxList)Syntax.E(syntax)).ToArray<Syntax>();
        int formLength = subForms.Length;
        if (subForms.Length is not (2 or 3)) {
            throw new Exception($"bad syntax in define @ {syntax.SrcLoc}: expected 2 or 3 sub-forms, got {formLength}: {Syntax.ToDatum(syntax).Print()}");
        }
        
        if (subForms[1] is ParsedVariable parsedVar) {
                return formLength == 3 ?
                    new ParsedDefine(
                        subForms[0],
                        parsedVar,
                        context.Expand(subForms[2]), // TODO: use new context that is defines not allowed
                        syntax.SrcLoc) :
                    new ParsedDefine(
                        subForms[0],
                        parsedVar,
                        syntax.SrcLoc);
            
        }

        throw new Exception($"ParseDefineForm: expected variable to have been parsed in first pass of expansion ");



    }
}
