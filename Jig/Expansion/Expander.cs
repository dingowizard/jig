using Jig;
namespace Jig.Expansion;

public class Expander
{

    public Expander(IEvaluator owner, IEvaluatorFactory evaluatorFactory)
    {
        Owner = owner;
        _evaluator = new Lazy<IEvaluator>(evaluatorFactory.Build);
    }
    
    public IEvaluator Owner { get; }
    
    private Lazy<IEvaluator> _evaluator;
    
    public IEvaluator Evaluator => _evaluator.Value;

    public IEnumerable<ParsedForm> ExpandSequence(IEnumerable<Syntax> syntaxes, ExpansionContext context) {
        Syntax[] forSecondPass = DoFirstPass(syntaxes, context).ToArray();
        foreach (Syntax syntax in forSecondPass) {
            yield return Expand(syntax, context);
        }
        
    }

    private Syntax DoFirstPassOneForm(Syntax stx, ExpansionContext context) {
        var form = Syntax.E(stx);
        if (form is SyntaxList.NonEmpty { First: Identifier kw } stxList) {
            switch (kw.Symbol.Name) {
                case "define":
                    if (stxList.Rest is not SyntaxList.NonEmpty { First: Identifier variable } rest)
                        throw new Exception($"malformed define: {Syntax.ToDatum(stx).Print()} ");
                    Parameter binding;
                    if (context.TryResolve(variable, out var parameter)) {
                        // we might be redefining something
                        binding = parameter;
                    } else {
                        binding = new Parameter(
                            variable.Symbol,
                            context.ScopeLevel,
                            context.VarIndex++,
                            variable.SrcLoc);
                        context.AddBinding(variable, binding);
                    }
                    var parsedVar = new ParsedVariable.TopLevel(variable, binding, variable.SrcLoc);
                    return new Syntax(
                        SyntaxList
                            .FromParams(kw, parsedVar)
                            .Concat<Syntax>(rest.Rest)
                            .ToSyntaxList(),
                        stx.SrcLoc);
                case "define-syntax":
                    if (stxList.Rest is not SyntaxList.NonEmpty { First: Identifier vr } rt)
                        throw new Exception($"malformed define: {Syntax.ToDatum(stx).Print()} ");
                    var bg = new Parameter(
                        vr.Symbol,
                        context.ScopeLevel,
                        context.VarIndex++,
                        vr.SrcLoc);
                    context.AddBinding(vr, bg);
                    var parsedKW = new ParsedVariable.TopLevel(vr, bg, vr.SrcLoc);
                    DefineSyntax(parsedKW, rt.Rest, context);
                    return new Syntax(
                        SyntaxList
                            .FromParams(kw, parsedKW)
                            .Concat<Syntax>(rt.Rest)
                            .ToSyntaxList(),
                        stx.SrcLoc);
                default:
                    return stx;
            }
        }

        return stx;
    }

    public void DefineSyntax(ParsedVariable kw, SyntaxList stxs, ExpansionContext context) {
        // _syntaxEnvironment.Add(id, rule);
        if (stxs is not SyntaxList.NonEmpty stxList) {
            throw new Exception($"malformed define-syntax:expected a third subform");
        }
        Syntax transformerStx =
            stxList.First;
        
        // TODO: I think we might need a ParsedKeyword type?

        // Expand third subform
        ParsedLambda transformerLambdaExpr = this.Evaluator.Expander.Expand(transformerStx, context) as ParsedLambda ?? throw new Exception(); // TODO: actually this should use the phase 1 runtime
        var transformerProcedure = this.Evaluator.EvaluateTransformerExpression(transformerLambdaExpr, context); // TODO: ditto
        // TODO: should it use ParsedVar rather than id? for define as well?
        Owner.Keywords.Add(kw.Identifier, transformerProcedure);
            
    }
    private IEnumerable<Syntax> DoFirstPass(IEnumerable<Syntax> syntaxes, ExpansionContext context) {
        foreach (var syntax in syntaxes) {
            yield return DoFirstPassOneForm(syntax, context);
        }
    }

    public ParsedForm Expand(Syntax stx, IRuntime runtime) {
        // TODO: it's kind of silly that we have to pass around the runtime like this. Maybe
        // the Expander should have a reference to a toplevel expansion context?
        return Expand(stx, new ExpansionContext(Owner, runtime.RuntimeEnvironment.TopLevels.Keys));
    }

    public ParsedForm Expand(Syntax syntax, ExpansionContext context) {

        if (syntax is Syntax.Literal lit) {
            // TODO: something about void. See ParsedLiteral.TryParse ... Not really sure how/why it could be here _as_syntax_ ... 
            return new ParsedLiteral(new Identifier(new Symbol("quote")), lit, syntax.SrcLoc);
        }
        if (syntax is Identifier id) {
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
            if (stxList.First is Identifier kw) {
                if (Owner.Keywords.TryFind(kw, out IExpansionRule? rule)) {
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

    public ParsedForm ExpandREPLForm(Syntax stx, ExpansionContext context) {
        return Expand(DoFirstPassOneForm(stx, context), context);
    }

}