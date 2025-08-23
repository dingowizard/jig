namespace Jig.Expansion;

public partial class CoreParseRules {

    public static ParsedForm DefineSyntax(Syntax syntax, ExpansionContext context) {
        var subForms = ((SyntaxList)Syntax.E(syntax)).ToArray<Syntax>();
        int formLength = subForms.Length;
        if (subForms.Length is not 3) {
            throw new Exception($"bad syntax in define-syntax @ {syntax.SrcLoc}: expected 3 sub-forms, got {formLength}");
        }
        Syntax.Identifier id =
            subForms[1] as Syntax.Identifier ?? throw new Exception($"bad syntax in define-syntax @ {syntax.SrcLoc}: expected first sub-form to be an identifier. Got {subForms[1]}");
        
        // TODO: I think we might need a ParsedKeyword type?

        // TODO: This is not right.
        var binding = new Binding(id.Symbol, context.ScopeLevel, context.VarIndex++); // TODO: should it create a new binding, or find one and create only if none?
        ParsedVariable var = subForms[1] as ParsedVariable ?? throw new Exception();

        return new ParsedDefineSyntax(subForms[0], var, subForms[2]);

        // TODO: expand and then evaluate the third sub-form
    }
    
}

