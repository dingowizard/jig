namespace Jig.Expansion;

public partial class CoreParseRules {

    public static ParsedForm ParseDefineForm(Syntax syntax, ExpansionContext context) {

        var subForms = ((SyntaxList)Syntax.E(syntax)).ToArray<Syntax>();
        int formLength = subForms.Length;
        if (!(subForms.Length is 2 or 3)) {
            throw new Exception($"bad syntax in define @ {syntax.SrcLoc}: expected 2 or 3 sub-forms, got {formLength}: {Syntax.ToDatum(syntax).Print()}");
        }

        if (subForms[1] is Identifier id) {
            // TODO: I'd like to only be in this path when expanding forms from the REPL
            var binding = new Parameter(id.Symbol, context.ScopeLevel, context.VarIndex++, id.SrcLoc);
            context.AddBinding(id, binding);
            if (id.ScopeSet.Count != 0) {
                // not top level
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
                    new ParsedVariable.TopLevel(id, binding, id.SrcLoc),
                    context.Expand(subForms[2]), // TODO: use new context that is defines not allowed
                    syntax.SrcLoc) :
                new ParsedDefine(
                    subForms[0],
                    new ParsedVariable.TopLevel(id, binding, id.SrcLoc),
                    syntax.SrcLoc);
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
        throw new Exception($"malformed define: expected second form to be a symbol. Got {subForms[0]}");



    }
}
