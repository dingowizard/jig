using System.Collections;
using System.Linq;
using System.Diagnostics.CodeAnalysis;

namespace Jig;

public class Syntax : Expr {
    // TODO: should this be an interface rather than a class? then e.g. Identifier : Symbol, ISyntaxObject
    //
    // NOTE:
    // in racket, syntax-objects do not have to have srclocs. a syntx-object could return #f for syntax-line or syntax-source

    public static Expr E(Syntax stx) {
        return stx.Expression;
    }

    public static Expr ToDatum(Syntax stx) {
        Expr x = Syntax.E(stx);
        if (x is IPair stxPair) {
            // Console.WriteLine($"Syntax.ToDatum: {stx}");
            return SyntaxPairToDatum(stxPair);
        }
        return x;
    }

    internal static void AddScope(Syntax stx, Scope scope) {
        if (stx is Syntax.Identifier id) {
            bool added = id.ScopeSet.Add(scope);
            // if (added) {
            //     Console.WriteLine($"AddScope: {scope} was added.");
            //     Console.WriteLine($"AddScope: ScopeSet contains {id.ScopeSet.Count}");
            //     foreach (var sc in id.ScopeSet) {
            //         Console.WriteLine ($"{sc} in ScopeSet Equals scope to add: {sc.Equals(scope)}");
            //         Console.WriteLine ($"sc.GetHashCode == scope.GetHashCode() : {sc.GetHashCode() == scope.GetHashCode()}");
            //     }
            // }
            return;
        }
        if (Syntax.E(stx) is SyntaxList stxList) {
            stxList.ToList<Syntax>().ForEach(s => AddScope(s, scope));
            return;
        }
        return;
    }

    internal static void RemoveScope(Syntax stx, Scope scope) {
        if (stx is Syntax.Identifier id) {
            id.ScopeSet.Remove(scope);
            return;
        }
        if (Syntax.E(stx) is SyntaxList stxList) {
            stxList.ToList<Syntax>().ForEach(s => RemoveScope(s, scope));
            return;
        }
        return;
    }

    internal static void ToggleScope(Syntax stx, Scope scope) {
        if (stx is Syntax.Identifier id) {
            if (!id.ScopeSet.Remove(scope)) {
                // NOTE: HashSet.Remove returns false if it does not find the item
                id.ScopeSet.Add(scope);
            }

        }
        if (Syntax.E(stx) is SyntaxList stxList) {
            stxList.ToList<Syntax>().ForEach(s => ToggleScope(s, scope));
            return;
        }
        return;
    }

    public static bool ToList(Syntax stx, [NotNullWhen(returnValue: true)] out List? stxList) {
        Expr e = Syntax.E(stx);
        if (e is SyntaxList slist) {
            stxList = slist;
            return true;
        } else if (e is List l) {
            if (l is List.NullType) {
                stxList = List.Empty;
                return true;

            }
            IEnumerable<Syntax> xs = l.Select(x => new Syntax(x, null));
            stxList = (SyntaxList)SyntaxList.FromIEnumerable(xs);
            return true;
        }
        stxList = null;
        return false;

    }

    public static Syntax FromDatum(SrcLoc? srcLoc, Expr x) {
        switch (x) {
            case Syntax stx:
                return stx;
            case Symbol sym:
                return new Identifier(sym, srcLoc);
            case List list:
                return new Syntax(SyntaxList.FromIEnumerable(list.Select(x => Syntax.FromDatum(srcLoc, x))), srcLoc);
            case IPair pair:
                return new Syntax((Expr)Expr.Pair.Cons(FromDatum(srcLoc, pair.Car),
                                                 FromDatum(srcLoc, pair.Cdr)),
                                  srcLoc);
            default:
                return new Syntax(x, srcLoc);
        }
    }


    public Syntax(Expr expr, SrcLoc? srcLoc = null) {
        Expression = expr;
        SrcLoc = srcLoc;
    }

    public class Literal : Syntax {
        internal Literal(Expr x, SrcLoc srcLoc) : base (x, srcLoc) {}
    }

    public class Identifier : Syntax {
        internal Identifier(Expr.Symbol symbol, SrcLoc? srcLoc = null) : base (symbol, srcLoc) {
            ScopeSet = new HashSet<Scope>();
        }

        // public static implicit operator Expr.Symbol(Identifier i) => i.Symbol;

        public new Expr.Symbol Symbol {
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
                if (ScopeSet.Equals(id.ScopeSet)) return true;
                return false;

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

        internal HashSet<Scope> ScopeSet {get; private set;}
    }

    public override string Print() => $"#<syntax: {ToDatum(this).Print()}>";

    public override string ToString() => $"#<syntax: {ToDatum(this).ToString()}>";

    private static Expr SyntaxPairToDatum(IPair stxPair) {
            // Syntax car = stxPair.Car as Syntax ??
            //     throw new Exception($"SyntaxObject.SyntaxPairToDatum: expected syntax pair, but car -- {stxPair.Car} -- is not a syntax object");
            Expr car = stxPair.Car is Syntax stxCar ? ToDatum(stxCar) : stxPair.Car;
            if (stxPair.Cdr is Expr.NullType) {
                return (Expr)Expr.Pair.Cons(car, (Expr)List.Empty);
            } else if (stxPair.Cdr is Syntax soCdr) {
                return (Expr)Expr.Pair.Cons(car, ToDatum(soCdr));
            } else if (stxPair.Cdr is IPair cdrPair) {
                return (Expr)Expr.Pair.Cons(car, SyntaxPairToDatum(cdrPair));
            } else {
                throw new Exception($"SyntaxPairToDatum: cdr of a syntax pair should be a syntax object, null or a pair. {stxPair.Cdr} is none of these)");
            }
    }

    protected virtual Expr Expression {get;}

    public SrcLoc? SrcLoc {get;}
    // public LexicalContext LexicalContext {get;}

}

public struct SrcLoc {
    public SrcLoc(string src, int line, int column, int position, int span) {
        Source = src;
        Line = line;
        Column = column;
        Position = position;
        Span = span;

    }
    public static SrcLoc WithNewEnd(SrcLoc start, int endPosition) {
        return new SrcLoc(start.Source, start.Line, start.Column, start.Position, endPosition - start.Position);
    }

    public override string ToString() => $"(srcloc {Source} {Line} {Column} {Position} {Span})";

    public string Source;
    public int Line;
    public int Column;
    public int Position;
    public int Span;

    internal static SrcLoc Combine(SrcLoc first, SrcLoc last) {
        return new SrcLoc(first.Source,
                              first.Line,
                              first.Column,
                              first.Position,
                              (last.Position - first.Position) + last.Span);    }
}

public class SyntaxList : List.NonEmpty, IEnumerable<Syntax> {

    public SyntaxList(Syntax car, List cdr) : base(car, cdr) {
        First = car;
    }

    public static List FromIEnumerable(IEnumerable<Syntax> stxs) {
        List result = List.Empty;
        for (int index = stxs.Count() - 1; index >= 0; index--) {
            result = new SyntaxList(stxs.ElementAt(index), result);
        }
        return result;
    }

    public static List FromParams(params Syntax[] stxs) {
        List result = List.Empty;
        for (int index = stxs.Count() - 1; index >= 0; index--) {
            result = new SyntaxList(stxs.ElementAt(index), result);
        }
        return result;
    }

    public Syntax First {get;}

    public new IEnumerator<Syntax> GetEnumerator() {
        List theList = this;
        while (theList is SyntaxList nonEmptyList) {
            yield return nonEmptyList.First;
            theList = nonEmptyList.Rest;
        }

    }

    IEnumerator IEnumerable.GetEnumerator() {
        return this.GetEnumerator();
    }


}

internal struct Scope {
    static int count = 0;
    int me;
    public Scope () {
        me = count ++;
    }
    public override int GetHashCode() => me;
    public override string ToString() => "scope" + me.ToString();
    public override bool Equals(object? obj) {
        if (obj is null) return false;
        if (obj is Scope sc) {
            return me == sc.me;

        } else {
            return false;
        }
    }
}
