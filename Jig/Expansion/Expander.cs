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

    private IEnumerable<Syntax> DoFirstPass(IEnumerable<Syntax> syntaxes, ExpansionContext context) {
        // In order for it to know what is a definition and what is an expression, the expander has
        // to expand macro uses by applying transformers, because we could have a transformer that does
        // something like take "(oof 2 b)" and expand it to "(define b 2)" for example.
        // It should refrain however from fully expanding some keyword forms like if, set!, lambda, etc
        // The expander needs to process all define forms and define syntax forms before doing certain
        // expansion tasks (such as the right hand side of a definition, as one example).
        // This is important for example when there are mutually-recursive function definitions.
        // The variable use of the procedure defined later inside the definition of the procedure defined
        // first will not be parsed correctly otherwise. Also any transformers defined in the block need
        // to be defined before their first use by the expander. These are the reasons for the first-pass.
        // During the first pass, the expander proceeds as follows:
        // For each form in the body: if the form is a list, if the first element of the list is an
        // identifier, if the identifier is bound to a transformer in the syntax environment,
        // then apply the transformer. Then apply the first-pass expander to the result of that application.
        // If the form is a define, then try to find the binding for the variable, if there is no binding,
        // make one, and record the new binding. If the form is a define-syntax, then find or make the binding
        // for the variable and evaluate the RHS in the phase n+1 environment (where n is the current phase)
        // and bind the value to the variable in the current syntax environment.
        // Begin forms are "spliced in" -- the first level of subforms become forms at the level where
        // the begin occurs. In other words, if the begin is body-level in a library then its first
        // level of subforms become body-level.
        // These subforms have to be processed in the manner described above
        // -- expand macro uses, register define form variables, evaluate and bind define syntaxes.
        // In certain bodies (libraries and lambdas) definitions are not allowed after the first
        // expression in the sequence. This will be caught as a syntax error in the first of the expander.
        // I think I'd rather do this in the second pass after splicing in the begin -- the second pass
        // would be dealing with a different list, one that doesn't include begin forms at the body level
        foreach (var syntax in syntaxes) {
            yield return DoFirstPassOneForm(syntax, context);
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
                    
                    // Expand third subform
                    if (rt.Rest is not SyntaxList.NonEmpty tail) {
                        throw new Exception($"malformed define-syntax:expected a third subform");
                    }
                    Syntax transformerStx = tail.First;
        
                    // TODO: I think we might need a ParsedKeyword type?

                    ParsedLambda transformerLambdaExpr = this.Evaluator.Expander.Expand(transformerStx, context) as ParsedLambda ?? throw new Exception(); // TODO: actually this should use the phase 1 runtime
                    DefineSyntax(parsedKW, transformerLambdaExpr, context);

                    return new ParsedDefineSyntax(vr, parsedKW, transformerLambdaExpr);
                default:
                    return stx;
            }
        }

        return stx;
    }

    public void DefineSyntax(ParsedVariable kw, ParsedLambda transformerLambdaExpr, ExpansionContext context) {
        // _syntaxEnvironment.Add(id, rule);
        var transformerProcedure = this.Evaluator.EvaluateTransformerExpression(transformerLambdaExpr, context); // TODO: ditto
        // TODO: should it use ParsedVar rather than id? for define as well?
        Owner.Keywords.Add(kw.Identifier, transformerProcedure);
            
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

        if (syntax is ParsedDefineSyntax defineSyntax)
        {
            // TODO: why does this need a special case?
            // Syntax.E(defineSyntax) is not a SyntaxList.NonEmpty. why not?
            return defineSyntax;
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
        
        
        throw new NotImplementedException($"Expand: doesn't know what to do with {Syntax.E(syntax).Print()} a {Syntax.E(syntax).GetType()}");
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