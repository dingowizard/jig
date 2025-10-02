namespace Jig.Expansion;
public class Parameter : Identifier {

    public Parameter(Symbol sym, int scopeLevel, int varIndex, SrcLoc? srcLoc) : base(sym, srcLoc)
    {
        // TODO: remove Index?
        ScopeLevel = scopeLevel;
        VarIndex = varIndex;
    }

    public int VarIndex { get; }

    public int ScopeLevel { get;}

    public int Index { get; }
    //TODO: why can't scope be like this? (scope needs a member to work. maybe because it has to define gethashcode and equals?)
    //TODO: should the binding contain the scope that it comes from?
    //TODO: can Scope and binding classes be combined in some way?
    public override string ToString() => $"binding{Index}";

    public override bool Equals(object? obj) {
        if (obj is null) return false;
        return obj switch {
            Parameter binding => this.Index == binding.Index,
            _ => false
        };
    }

    protected bool Equals(Parameter other) {
        return Index == other.Index;
    }

    public override int GetHashCode() {
        return Index;
    }
}
