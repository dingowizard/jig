using Jig;
namespace Jig.Expansion;

public class Expander {

    public IEnumerable<ParsedForm> ExpandFile(IEnumerable<Syntax> syntaxes, ExpansionContext context) {
        Syntax[] forSecondPass = DoFirstPass(syntaxes, context).ToArray();
        foreach (Syntax syntax in forSecondPass) {
            yield return Expand(syntax, context);
        }
        
    }
    private IEnumerable<Syntax> DoFirstPass(IEnumerable<Syntax> syntaxes, ExpansionContext context) {
        foreach (var syntax in syntaxes) {
            var form = Syntax.E(syntax);
            if (form is SyntaxList.NonEmpty {First: Syntax.Identifier kw} stxList) {
                switch (kw.Symbol.Name) {
                    case "define":
                        if (stxList.Rest is not SyntaxList.NonEmpty {First: Syntax.Identifier variable} rest)
                            throw new Exception($"malformed define: {Syntax.ToDatum(syntax).Print()} ");
                        var binding = new Binding(
                            variable.Symbol,
                            context.ScopeLevel,
                            context.VarIndex++);
                        context.AddBinding(variable, binding);
                        var parsedVar = new ParsedVariable.TopLevel(variable, binding, variable.SrcLoc);
                        yield return new Syntax(
                            SyntaxList
                                .FromParams(kw, parsedVar)
                                .Concat<Syntax>(rest.Rest)
                                .ToSyntaxList(),
                            syntax.SrcLoc);
                        break;
                    case "define-syntax":
                        if (stxList.Rest is not SyntaxList.NonEmpty {First: Syntax.Identifier vr} rt)
                            throw new Exception($"malformed define: {Syntax.ToDatum(syntax).Print()} ");
                        var bg = new Binding(
                            vr.Symbol,
                            context.ScopeLevel,
                            context.VarIndex++);
                        context.AddBinding(vr, bg);
                        var parsedKW = new ParsedVariable.TopLevel(vr, bg, vr.SrcLoc);
                        context.DefineSyntax(parsedKW, rt.Rest);
                        yield return new Syntax(
                            SyntaxList
                                .FromParams(kw, parsedKW)
                                .Concat<Syntax>(rt.Rest)
                                .ToSyntaxList(),
                            syntax.SrcLoc);
                        break;
                    default:
                        yield return syntax;
                        break;
                }
            } else {
                yield return syntax;
            }
        }
    }

    public ParsedForm Expand(Syntax syntax, ExpansionContext context) {

        if (syntax is Syntax.Literal lit) {
            // TODO: something about void. See ParsedLiteral.TryParse ... Not really sure how/why it could be here _as_syntax_ ... 
            return new ParsedLiteral(new Syntax.Identifier(new Form.Symbol("quote")), lit, syntax.SrcLoc);
        }
        if (syntax is Syntax.Identifier id) {
            // TODO: error if id is keyword in expansion env?
            // resolve id, return parsed var
            if(context.TryResolve(id, out var binding)) {
                if (binding.ScopeLevel == 0) {
                    return new ParsedVariable.TopLevel(id, binding, syntax.SrcLoc);
                }
                return new ParsedVariable.Lexical(id, binding, syntax.SrcLoc);
            }
            throw new Exception($"could not resolve identifier {id.Symbol.Print()} @ {id.SrcLoc}");

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
        System.Collections.Generic.List<ParsedForm> elements = [];
        elements.AddRange(stxList.Cast<Syntax>().Select(stx => Expand(stx, context)));
        return new ParsedApplication(
            elements,
            srcLoc);
    }
    
}