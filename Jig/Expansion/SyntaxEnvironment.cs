using System.Diagnostics.CodeAnalysis;
namespace Jig.Expansion;

public abstract class SyntaxEnvironment {

    static SyntaxEnvironment() {
        var coreForms = new Dictionary<Symbol, IExpansionRule>();
        CoreForms = new TopLevelSyntaxEnvironment(coreForms);
        // TODO: these should be identifiers, not symbols, but they need to be resolved correctly
        // TODO: probably we don't want to create new identifiers and symbols ...
        // TODO: the ids should have source locations -- names not rows and columns
        coreForms.Add(new Symbol("begin"), new CoreSyntaxRule(CoreParseRules.ParseBeginForm));
        coreForms.Add(new Symbol("define"), new CoreSyntaxRule(CoreParseRules.ParseDefineForm));
        coreForms.Add(new Symbol("define-syntax"), new CoreSyntaxRule(CoreParseRules.DefineSyntax));
        coreForms.Add(new Symbol("if"), new CoreSyntaxRule(CoreParseRules.ParseIfForm));
        coreForms.Add(new Symbol("lambda"), new CoreSyntaxRule(CoreParseRules.ParseLambdaForm));
        coreForms.Add(new Symbol("quote"), new CoreSyntaxRule(CoreParseRules.ParseQuoteForm));
        coreForms.Add(new Symbol("quote-syntax"), new CoreSyntaxRule(CoreParseRules.ParseQuoteSyntaxForm));
        coreForms.Add(new Symbol("set!"), new CoreSyntaxRule(CoreParseRules.ParseSetForm));
        
        var defaultTransformers = new Dictionary<Symbol, IExpansionRule>();
        // add builtin macros here
        defaultTransformers.Add(new Symbol("and"), new BuiltinTransformer(BuiltinTransformer.and));
        defaultTransformers.Add(new Symbol("quasiquote"), new BuiltinTransformer(BuiltinTransformer.quasiquote));
        Default = new TopLevelSyntaxEnvironment(coreForms.Concat(defaultTransformers).ToDictionary(kv => kv.Key, kv => kv.Value));

    }
    public static TopLevelSyntaxEnvironment CoreForms {get;}
    public static TopLevelSyntaxEnvironment Default {get;}

    public bool TryFind(Syntax.Identifier keyword, [NotNullWhen(returnValue: true)] out IExpansionRule expansionRule) {
        if (_rules.TryGetValue(keyword.Symbol, out var rule)) {
            expansionRule = rule;
            return true;
        }
        if (this is ScopedSyntaxEnvironment nested) {
            return nested.Parent.TryFind(keyword, out expansionRule);
        } else {
            expansionRule = null;
            return false;
        }

    }

    public SyntaxEnvironment Extend() {
        return new ScopedSyntaxEnvironment(this, new Dictionary<Symbol, IExpansionRule>());
    }

    public SyntaxEnvironment Extend(Dictionary<Symbol, IExpansionRule> rules) {
        return new ScopedSyntaxEnvironment(this, rules);
    }

    protected abstract Dictionary<Symbol, IExpansionRule> _rules {get;}
    public abstract void Add(Syntax.Identifier kw, IExpansionRule expansionRule);

}

public class TopLevelSyntaxEnvironment(Dictionary<Symbol, IExpansionRule> rules) : SyntaxEnvironment {

    protected override Dictionary<Symbol, IExpansionRule> _rules {get;} = rules;
    public override void Add(Syntax.Identifier kw, IExpansionRule expansionRule) {
        _rules.Add(kw.Symbol, expansionRule);
    }

}

public class ScopedSyntaxEnvironment(SyntaxEnvironment parent, Dictionary<Symbol, IExpansionRule> rules) : SyntaxEnvironment {

    protected override Dictionary<Symbol, IExpansionRule> _rules {get;} = rules;
    public override void Add(Syntax.Identifier kw, IExpansionRule expansionRule) {
        _rules.Add(kw.Symbol, expansionRule);
    }

    public SyntaxEnvironment Parent = parent;
}
