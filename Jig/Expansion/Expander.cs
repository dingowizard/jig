namespace Jig.Expansion;

public class Expander {

    public ParsedExpr Expand(Syntax syntax, ExpansionContext context) {

        if (syntax is Syntax.Literal lit) {
            return new ParsedLiteral(new Syntax.Identifier(new Form.Symbol("quote")), lit, syntax.SrcLoc);
        }
        // TODO: something about void. See ParsedLiteral.TryParse ... Not really sure how/why it could be here _as_syntax_ ... 
        if (syntax is Syntax.Identifier id) {
            // TODO: error if id is keyword in expansion env?
            // resolve id, return parsed var
            if(context.TryResolve(id, out var binding)) {
                id.Symbol.Binding = binding; // TODO: shouldn't it only be the ParsedVar that has a binding?
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