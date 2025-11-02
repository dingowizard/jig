namespace Jig.Expansion;
public class Parameter : Identifier {

    public Parameter(Symbol sym, HashSet<Scope> scopeSet, int scopeLevel, int varIndex, SrcLoc? srcLoc)
        : base(sym, scopeSet, srcLoc) {
        // TODO: remove Index?
        ScopeLevel = scopeLevel;
        Index = varIndex;
    }


    public int ScopeLevel { get;}

    public int Index { get; }
    //TODO: why can't scope be like this? (scope needs a member to work. maybe because it has to define gethashcode and equals?)
    //TODO: should the binding contain the scope that it comes from?
    //TODO: can Scope and binding classes be combined in some way?
    public override string ToString() => $"binding{Index}";

    public override bool Equals(object? obj) {
        if (obj is null) return false;
        return obj switch {
            Parameter binding => this.Symbol.Equals(binding.Symbol) && this.Index == binding.Index && this.ScopeLevel == binding.ScopeLevel,
            _ => false,
        };
    }


    public override int GetHashCode()
    {
        return HashCode.Combine(Symbol, ScopeLevel, Index);
    }
}
