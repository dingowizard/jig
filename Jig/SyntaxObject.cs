using System;
using System.Collections;
using System.Diagnostics.CodeAnalysis;
using System.Numerics;

namespace Jig;

public class Syntax : Form {
    // TODO: should this be an interface rather than a class? then e.g. Identifier : Symbol, ISyntaxObject
    //
    // NOTE:
    // in racket, syntax-objects do not have to have srclocs. a syntx-object could return #f for syntax-line or syntax-source

    public static Form E(Syntax stx) {
        return stx.Expression;
    }

    public static Syntax Cons(Syntax car, Form cdr) {
        return new Syntax((Form)Form.Pair.Cons(car, cdr));
    }

    public static Form ToDatum(Syntax stx) {
        Form x = Syntax.E(stx);
        if (x is IPair stxPair) {
            // Console.WriteLine($"Syntax.ToDatum: {stx}");
            return SyntaxPairToDatum(stxPair);
        }
        return x;
    }

    internal static void AddScope(Syntax stx, Scope scope) {
        if (stx is Syntax.Identifier id) {
            bool added = id.ScopeSet.Add(scope);
            // if (added && id.Symbol.Name == "a") {
            //     Console.WriteLine($"AddScope: {scope} was added to {stx}.");
            //         Console.WriteLine($"\tat {id.SrcLoc.ToString() ?? "null"}");
                // Console.WriteLine($"AddScope: ScopeSet contains {id.ScopeSet.Count}");
                // foreach (var sc in id.ScopeSet) {
                //     Console.WriteLine ($"{sc} in ScopeSet Equals scope to add: {sc.Equals(scope)}");
                //     Console.WriteLine ($"sc.GetHashCode == scope.GetHashCode() : {sc.GetHashCode() == scope.GetHashCode()}");
                // }
            // }
            return;
        }
        if (Syntax.E(stx) is SyntaxList stxList) {
            stxList.ToList<Syntax>().ForEach(s => AddScope(s, scope));
            return;
        }
        if (Syntax.E(stx) is IPair pair) {
            if (pair.Car is Syntax syntax && pair.Cdr is Syntax syntax1) {
                AddScope(syntax, scope);
                AddScope(syntax1, scope);
                return;
            }
            // TODO: need to handle the case where car or cdr is SyntaxPair. In general, figure out SyntaxPairs
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
        if (Syntax.E(stx) is IPair pair) {
            RemoveScope((Syntax)pair.Car, scope);
            RemoveScope((Syntax)pair.Cdr, scope);
            return;
        }
        return;
    }

    internal static void ToggleScope(Syntax stx, Scope scope) {
        // TODO: AAAAARRRGGH!
        if (stx is Syntax.Identifier id) {
            if (!id.ScopeSet.Remove(scope)) {
                id.ScopeSet.Add(scope);
            } 
            return;

        }
        if (Syntax.E(stx) is SyntaxList stxList) {
            stxList.ToList<Syntax>().ForEach(s => ToggleScope(s, scope));
            return;
        }
        if (Syntax.E(stx) is IPair pair) {
            ToggleScope((Syntax)pair.Car, scope);
            if (pair.Cdr is Syntax stxCdr) {
                ToggleScope(stxCdr, scope);

            } else if (pair.Cdr is SyntaxPair stxPairCdr) {
                ToggleScope(stxPairCdr.Car, scope);
                ToggleScope(stxPairCdr.Cdr, scope);
            } else if (pair.Cdr is List.NonEmpty l) {
                foreach(var x in l) {
                    ToggleScope((Syntax)x, scope);
                }
                return;
            } else {
                throw new Exception($"In ToggleScope: unhandled IPair cdr: {pair.Cdr}, a {pair.Cdr.GetType()}");
            }
            return;

        }
        return;
    }

    public static bool ToList(Syntax stx, [NotNullWhen(returnValue: true)] out List? stxList) {
        Form e = Syntax.E(stx);
        if (e is SyntaxList slist) {
            stxList = slist;
            return true;
        } else if (e is List l) {
            if (l is List.Empty) {
                stxList = List.Null;
                return true;

            }
            IEnumerable<Syntax> xs = l.Select(x => new Syntax(x, null));
            stxList = (SyntaxList)SyntaxList.FromIEnumerable(xs);
            return true;
        }
        stxList = null;
        return false;

    }

    private static List FlattenPair(Syntax car, SyntaxList cdr) {
        switch (Syntax.E(car)) {
            case List.Empty: return FlattenPair(cdr.First, cdr.Cdr);
            case IPair pair:
                Syntax pairCar = pair.Car as Syntax ?? throw new Exception($"FlattenPair: expected pair.Car to be Syntax but got {pair.Car.Print()}, a {pair.Car.GetType()}");
                return (List)FlattenPair(pairCar, pair.Cdr).Append(Flatten(new Syntax(cdr)));
            default: return (List)Form.Pair.Cons(car, Flatten(new Syntax(cdr)));
        }

    }

    private static List FlattenPair(Syntax car, Form cdr) {
        if (cdr is SyntaxList stxList) {
            return FlattenPair(car, stxList);
        }
        if (cdr is List.Empty){
            return Flatten(car);
        }
        Syntax stxCdr = cdr is Syntax stx ? stx : cdr is SyntaxList slist ? new(slist) : throw new Exception($"{cdr}");
        return Syntax.E(car) switch
        {
            List.Empty => Flatten(stxCdr),
            IPair pair => (List)FlattenPair((Syntax)pair.Car, pair.Cdr).Append(Flatten(stxCdr)),
            _ => (SyntaxList)Form.Pair.Cons(car, Flatten(stxCdr)),
        };
    }

    public static List Flatten(Syntax stx) {
        Console.WriteLine($"flatten: stx = {stx}");
        switch(Syntax.E(stx)) {
            case IPair pair:
                Syntax car = pair.Car as Syntax ?? throw new Exception();
                return FlattenPair(car, pair.Cdr);
            case List.Empty: return List.Null;
            default:
                // this should catch any non null atoms
                return SyntaxList.FromParams(stx);
        }

    }

    public static Thunk? syntax_flatten(Delegate k, List args) {
        if (args.Count() != 1) return Builtins.Error(k, $"syntax-flatten: expected one argument.");
        if (args.ElementAt(0) is Syntax stx) {
            return Continuation.ApplyDelegate(k, Flatten(stx));
        } else {
            return Builtins.Error(k, $"syntax-flatten: expected syntax argument.");
        }
    }

    public static Syntax FromDatum(SrcLoc? srcLoc, Form x) {
        switch (x) {
            case Syntax stx:
                return stx;
            case Symbol sym:
                return new Identifier(sym, srcLoc);
            case Form.Bool:
            case Form.Char:
            case Form.DoubleNumber:
            case Form.IntegerNumber:
            case Form.String:
            case Form.Vector:
                return new Literal(x, srcLoc);
            case List list:
                return new Syntax(SyntaxList.FromIEnumerable(list.Select(x => Syntax.FromDatum(srcLoc, x))), srcLoc);
            case IPair pair:
                return new Syntax((Form)Form.Pair.Cons(FromDatum(srcLoc, pair.Car),
                                                 FromDatum(srcLoc, pair.Cdr)),
                                  srcLoc);
            default:
                return new Syntax(x, srcLoc);
        }
    }


    public Syntax(Form expr, SrcLoc? srcLoc = null) {
        Expression = expr;
        SrcLoc = srcLoc;
    }

    public class Literal : Syntax {
        internal Literal(Form x, SrcLoc? srcLoc = null) : base (x, srcLoc) {}
    }

    public class Identifier : Syntax {
        internal Identifier(Form.Symbol symbol, SrcLoc? srcLoc = null) : base (symbol, srcLoc) {
            ScopeSet = new HashSet<Scope>();
        }

        // public static implicit operator Expr.Symbol(Identifier i) => i.Symbol;

        public new Form.Symbol Symbol {
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

    public override string ToString() => $"#<syntax: {ToDatum(this).Print()}>";

    private static Form SyntaxPairToDatum(IPair stxPair) {
            // Syntax car = stxPair.Car as Syntax ??
            //     throw new Exception($"SyntaxObject.SyntaxPairToDatum: expected syntax pair, but car -- {stxPair.Car} -- is not a syntax object");
            Form car = stxPair.Car is Syntax stxCar ? ToDatum(stxCar) : stxPair.Car;
            if (stxPair.Cdr is List.Empty) {
                return (Form)Form.Pair.Cons(car, (Form)List.Null);
            } else if (stxPair.Cdr is Syntax soCdr) {
                return (Form)Form.Pair.Cons(car, ToDatum(soCdr));
            } else if (stxPair.Cdr is IPair cdrPair) {
                return (Form)Form.Pair.Cons(car, SyntaxPairToDatum(cdrPair));
            } else {
                throw new Exception($"SyntaxPairToDatum: cdr of a syntax pair should be a syntax object, null or a pair. {stxPair.Cdr} is none of these)");
            }
    }

    protected virtual Form Expression {get;}

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
        List result = List.Null;
        for (int index = stxs.Count() - 1; index >= 0; index--) {
            result = new SyntaxList(stxs.ElementAt(index), result);
        }
        return result;
    }

    public static List FromParams(params Syntax[] stxs) {
        List result = List.Null;
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
