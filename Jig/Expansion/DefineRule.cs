namespace Jig.Expansion;

public partial class CoreParseRules {

    public static ParsedExpr ParseDefineForm(Syntax syntax, ExpansionContext context) {

        var subForms = ((SyntaxList)Syntax.E(syntax)).ToArray<Syntax>();
        int formLength = subForms.Length;
        if (!(subForms.Length is 2 or 3)) {
            throw new Exception($"bad syntax in define @ {syntax.SrcLoc}: expected 2 or 3 sub-forms, got {formLength}");
        }
        Syntax.Identifier id = subForms[1] as Syntax.Identifier
            ?? throw new Exception($"bad syntax in define @ {syntax.SrcLoc}: expected first sub-form to be an identifier. Got {subForms[1]}");

        // TODO: hm...
        if (id.ScopeSet.Count != 0) {
            // not top level
            var binding = new Binding(id.Symbol, context.ScopeLevel, context.VarIndex++); // TODO: should it create a new binding, or find one and create only if none?
            id.Symbol.Binding = binding;
            context.AddBinding(id, id.Symbol.Binding);
            return formLength == 3 ?
                new ParsedDefine(
                    subForms[0],
                    new ParsedVariable.Lexical(id, binding, id.SrcLoc),
                    context.Expand(subForms[2]), // TODO: use new context that is defines not allowed
                    syntax.SrcLoc) :
                new ParsedDefine(
                    subForms[0],
                    new ParsedVariable.Lexical(id, binding, id.SrcLoc),
                    syntax.SrcLoc);

        }

        return formLength == 3 ?
            new ParsedDefine(
                subForms[0],
                new ParsedVariable.TopLevel(id, id.SrcLoc),
                context.Expand(subForms[2]), // TODO: use new context that is defines not allowed
                syntax.SrcLoc) :
            new ParsedDefine(
                subForms[0],
                new ParsedVariable.TopLevel(id, id.SrcLoc),
                syntax.SrcLoc);
    }
}
