using System.Diagnostics.CodeAnalysis;
namespace Jig.Expansion;

public abstract class SyntaxEnvironment {

    public bool TryFind(Identifier keyword, [NotNullWhen(returnValue: true)] out IExpansionRule? expansionRule) {
        if (Rules.TryGetValue(keyword.Symbol, out var rule)) {
            expansionRule = rule;
            return true;
        } else {
            if (this is ScopedSyntaxEnvironment nested) {
                return nested.Parent.TryFind(keyword, out expansionRule);
            } else {
                expansionRule = null;
                return false;
            }
            
        }

    }

    public SyntaxEnvironment Extend() {
        return new ScopedSyntaxEnvironment(this, new Dictionary<Symbol, IExpansionRule>());
    }

    public abstract Dictionary<Symbol, IExpansionRule> Rules {get;}
    

    public abstract void Add(Identifier kw, IExpansionRule expansionRule);

}


public class TopLevelSyntaxEnvironment : SyntaxEnvironment {

    public TopLevelSyntaxEnvironment(Dictionary<Symbol, IExpansionRule> rules) {
        Rules = rules;
    }
    public TopLevelSyntaxEnvironment(IEnumerable<(Symbol Key, IExpansionRule Value)> pairs)
    {
        Rules = pairs.ToDictionary(pair => pair.Key, pair => pair.Value);
        
    }
    public override Dictionary<Symbol, IExpansionRule> Rules {get;}
    public override void Add(Identifier kw, IExpansionRule expansionRule) {
        Rules.Add(kw.Symbol, expansionRule);
    }

}

public class ScopedSyntaxEnvironment(SyntaxEnvironment parent, Dictionary<Symbol, IExpansionRule> rules) : SyntaxEnvironment {
    public override Dictionary<Symbol, IExpansionRule> Rules {get;} = rules;
    public override void Add(Identifier kw, IExpansionRule expansionRule) {
        Rules.Add(kw.Symbol, expansionRule);
    }

    public SyntaxEnvironment Parent = parent;
}
