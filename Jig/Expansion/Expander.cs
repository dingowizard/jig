namespace Jig.Expansion;

public class Expander
{

    public Expander(IEvaluator owner, IEvaluatorFactory evaluatorFactory) {
        Owner = owner;
        _evaluator = new Lazy<IEvaluator>(evaluatorFactory.Build);
    }
    
    public IEvaluator Owner { get; }
    
    private Lazy<IEvaluator> _evaluator;
    
    public IEvaluator Evaluator => _evaluator.Value;


    public ParsedForm Expand(Syntax syntax, ExpansionContext context) {
        
        return SecondPass(FirstPass(syntax, context), context);
        
    }

    internal ParsedForm SecondPass(SemiParsedForm firstPass, ExpansionContext context) {
        return firstPass.Expand(context);
    }

    internal SemiParsedForm FirstPass(Syntax syntax, ExpansionContext context) {
        
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
        while (true) {
            if (syntax is Syntax.Literal lit) {
                return new SemiParsedLiteral(lit);
            }
            if (syntax is Identifier id) {
                // This is a variable reference -- an expression. It is occuring either as body level expression
                // ins a sequence or in an argument context
                // It doesn't need to get processed until
                // second pass. When all the defines have been registered
                return new SemiParsedVar(id);

            }
            if (Syntax.E(syntax) is SyntaxList.NonEmpty stxList) {
                if (stxList.First is Identifier kw) {
                    if (Owner.Keywords.TryFind(kw, out IExpansionRule? rule)) {
                        // we found a keyword, so apply rule
                        var expanded = rule.Expand(syntax, context);
                        // TODO: expand for transformers should return Syntax, not ParsedForm
                        if (expanded is SemiParsedForm semi) {
                            return semi;
                        } else {
                            syntax = expanded;
                            continue;
                        }
                    }
                }
                return new SemiParsedApplication(stxList, syntax.SrcLoc );
            }

            throw new NotImplementedException($"couldn't expand {syntax.Print()}, a {syntax.GetType()}");

        }
    }

    public IEnumerable<ParsedForm> ExpandSequence(IEnumerable<Syntax> syntaxes, ExpansionContext context) {
        var semiParsed = syntaxes.Select(stx => FirstPass(stx, context)).ToList();
        return semiParsed.Select(s => SecondPass(s, context));
    }

    public ParsedForm ExpandREPLForm(Syntax syntax, ExpansionContext context) {
        return Expand(syntax, context);
    }
}

internal class SemiParsedApplication : SemiParsedExpression
{
    public SemiParsedApplication(SyntaxList.NonEmpty stxList, SrcLoc? syntaxSrcLoc) : base(stxList, syntaxSrcLoc) {
        SubForms = stxList.ToArray<Syntax>();
    }
    public Syntax[] SubForms {get; set;}

    public override ParsedForm Expand(ExpansionContext context) {
        var newContext = context.ExtendWithExpressionContext();
        var expanded = SubForms.Select(s => newContext.Expand(s)).ToArray();
        return new ParsedApplication(expanded, SrcLoc);
    }
}

internal class SemiParsedVar : SemiParsedExpression {
    public SemiParsedVar(Identifier id) : base(id.Expression, id.SrcLoc)
    {
        // TODO: you probably have to save other fields of id
        Identifier = id;
    }
    public Identifier Identifier {get;}

    public override ParsedForm Expand(ExpansionContext context)
    {
            if(context.TryResolve(Identifier, out var binding)) {
                if (binding.ScopeLevel == 0) {
                    return new ParsedVariable.TopLevel(Identifier, binding, Identifier.SrcLoc);
                }
                return new ParsedVariable.Lexical(Identifier, binding, Identifier.SrcLoc);
            }
            throw new Exception($"could not resolve Identifier {Identifier.Symbol.Print()} @ {Identifier.SrcLoc}");
    }
}

internal class SemiParsedLiteral : SemiParsedExpression
{
    public SemiParsedLiteral(Literal lit)  : base(lit.Expression, lit.SrcLoc) {
        Literal = lit;
    }
    public Literal Literal {get;}

    public override ParsedForm Expand(ExpansionContext context) {
        return new ParsedLiteral(new Identifier(new Symbol("quote")), Literal, Literal.SrcLoc);
    }
}

public abstract class SemiParsedForm : Syntax // note: has to be a syntax because this is what is returned by IExpansionRule
{
    public SemiParsedForm(ISchemeValue expr, SrcLoc? srcLoc = null) : base(expr, srcLoc)
    {
    }
    
    public abstract ParsedForm Expand(ExpansionContext context);
}
public abstract class SemiParsedDefinition : SemiParsedForm {
    public SemiParsedDefinition(ISchemeValue expr, SrcLoc? srcLoc = null) : base(expr, srcLoc) { }
    
}
public abstract class SemiParsedExpression : SemiParsedForm {
    public SemiParsedExpression(ISchemeValue expr, SrcLoc? srcLoc = null) : base(expr, srcLoc) { }
    
}
