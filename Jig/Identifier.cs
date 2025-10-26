namespace Jig;

public class Identifier : Syntax {
    public Identifier(Symbol symbol, SrcLoc? srcLoc = null) : base (symbol, srcLoc) {
        ScopeSet = new HashSet<Scope>();
    }

    public Identifier(Symbol symbol, HashSet<Scope> scopeSet, SrcLoc? srcLoc = null) : base(symbol, srcLoc) {
        ScopeSet = scopeSet;
    }
        

    // public static implicit operator Expr.Symbol(Identifier i) => i.Symbol;

    internal void AddScope(Scope sc) {
            
        bool added = ScopeSet.Add(sc);
        // if (added && id.Symbol.Name == "a") {
        //     Console.WriteLine($"AddScope: {scope} was added to {stx}.");
        //         Console.WriteLine($"\tat {id.SrcLoc.ToString() ?? "null"}");
        // Console.WriteLine($"AddScope: ScopeSet contains {id.ScopeSet.Count}");
        // foreach (var sc in id.ScopeSet) {
        //     Console.WriteLine ($"{sc} in ScopeSet Equals scope to add: {sc.Equals(scope)}");
        //     Console.WriteLine ($"sc.GetHashCode == scope.GetHashCode() : {sc.GetHashCode() == scope.GetHashCode()}");
        // }
        // }
    }

    public Symbol Symbol {
        get {
            return (Symbol)Expression;
        }
    }

    public override bool Equals(object? obj) {
        // if (Symbol.Name == "stx") {
        //     Console.WriteLine($"Equals: we're here!");
        // }
        if (obj is Identifier id) {
            if (!Symbol.Equals(id.Symbol)) return false;
            if (!ScopeSet.Equals(id.ScopeSet)) return false;
            return true;

        } else {
            return false;
        }

    }

    public override int GetHashCode() {
        // if (Symbol.Name == "stx") {
        // Console.WriteLine($"GetHashCode: we're here!");
        // }
        int hash = Symbol.GetHashCode();
        unchecked {
            hash = hash * 31 + ScopeSet.GetHashCode();
        }
        return hash;
    }

    public HashSet<Scope> ScopeSet {get; private set;}
}