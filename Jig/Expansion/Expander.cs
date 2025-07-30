using Jig;
namespace Jig.Expansion;

public class Expander {

    public IEnumerable<ParsedExpr> ExpandFile(IEnumerable<Syntax> syntaxes, ExpansionContext context) {
        IEnumerable<Syntax> forSecondPass = DoFirstPass(syntaxes, context);
        foreach (Syntax syntax in forSecondPass) {
            var parsedForm = Expand(syntax, context);
            yield return parsedForm;
        }
        
    }
    private IEnumerable<Syntax> DoFirstPass(IEnumerable<Syntax> syntaxes, ExpansionContext context) {
        foreach (var syntax in syntaxes) {
            var form = Syntax.E(syntax);
            if (form is SyntaxList.NonEmpty {First: Syntax.Identifier id} stxList) {
                switch (id.Symbol.Name) {
                    case "define":
                        if (stxList.Rest is not SyntaxList.NonEmpty {First: Syntax.Identifier variable} rest)
                            throw new Exception($"malformed define: {Syntax.ToDatum(syntax).Print()} ");
                        var binding = new Binding(
                            variable.Symbol,
                            context.ScopeLevel,
                            context.VarIndex++);
                        variable.Symbol.Binding = binding;
                        context.AddBinding(variable, variable.Symbol.Binding);
                        // TODO: this should handle lexical vars for first passes on lambda bodies
                        var parsedVar = new ParsedVariable.TopLevel(variable, variable.SrcLoc);
                        yield return new Syntax(
                            SyntaxList
                                .FromParams(id, parsedVar)
                                .Concat<Syntax>(rest)
                                .ToSyntaxList(),
                            syntax.SrcLoc);
                        throw new Exception($"malformed define: {Syntax.ToDatum(syntax).Print()} ");
                    default:
                        yield return syntax;
                        break;
                }
            } else {
                yield return syntax;
            }
        }
    }

    public ParsedExpr Expand(Syntax syntax, ExpansionContext context) {

        if (syntax is Syntax.Literal lit) {
            // TODO: something about void. See ParsedLiteral.TryParse ... Not really sure how/why it could be here _as_syntax_ ... 
            return new ParsedLiteral(new Syntax.Identifier(new Form.Symbol("quote")), lit, syntax.SrcLoc);
        }
        if (syntax is Syntax.Identifier id) {
            // TODO: error if id is keyword in expansion env?
            // resolve id, return parsed var
            if(context.TryResolve(id, out var binding)) {
                id.Symbol.Binding = binding; // TODO: shouldn't it only be the ParsedVar that has a binding?
                if (binding.ScopeLevel == 0) {
                    return new ParsedVariable.TopLevel(id, syntax.SrcLoc);
                }
                return new ParsedVariable.Lexical(id, binding, syntax.SrcLoc);
            }
            // TODO: toplevel vars still _do_ have a binding, right? a module binding?
            return new ParsedVariable.TopLevel(id, syntax.SrcLoc);
            
        }

        if (Syntax.E(syntax) is SyntaxList.NonEmpty stxList) {
            if (stxList.First is Syntax.Identifier kw) {
                if (context.TryFindKeyword(kw, out IExpansionRule? rule)) {
                    return rule.Expand(syntax, context);
                } else {
                    return ExpandApplication(stxList, context, syntax.SrcLoc);
                }
            } else {
                return ExpandApplication(stxList, context, syntax.SrcLoc);
            }


        }
        
        
        throw new NotImplementedException($"Expand: doesn't know what to do with a {Syntax.E(syntax).GetType()}");
    }

    private ParsedApplication ExpandApplication(SyntaxList.NonEmpty stxList, ExpansionContext context, SrcLoc? srcLoc) {
        
        // NOTE: idk why but passing ParsedApplication stxList.Select(Expand) causes a very weird bug
        // in which the lambda expression gets evaluated twice and chokes on adding the binding for the parameters twice
        System.Collections.Generic.List<ParsedExpr> elements = [];
        elements.AddRange(stxList.Cast<Syntax>().Select(stx => Expand(stx, context)));
        return new ParsedApplication(
            elements,
            srcLoc);
    }
    
}