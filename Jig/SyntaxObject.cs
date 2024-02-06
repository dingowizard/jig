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
            return SyntaxPairToDatum(stxPair);
        }
        return x;
    }

    public static Syntax FromDatum(Expr datum, SrcLoc srcLoc) {
        switch (datum) {
            case Expr.Boolean: case Expr.Integer: case Expr.Double:
                return new Literal(datum, srcLoc);
            case Expr.Symbol sym:
                return new Identifier(sym, srcLoc);
            default:
                return new Syntax(datum, srcLoc);
        }
    }

    public static bool ToList(Syntax stx, [NotNullWhen(returnValue: true)] out SyntaxList stxList) {
        throw new NotImplementedException();

    }


    public Syntax(Expr expr, SrcLoc srcLoc) {
        Expression = expr;
        SrcLoc = srcLoc;
    }

    public class Literal : Syntax {
        internal Literal(Expr x, SrcLoc srcLoc) : base (x, srcLoc) {}
    }

    public class Identifier : Syntax {
        internal Identifier(Expr.Symbol symbol, SrcLoc srcLoc) : base (symbol, srcLoc) {}

        public new Expr.Symbol Symbol {
            get {
                return (Symbol)Expression;
            }
        }
    }

    public override string Print() => $"#<syntax: {ToDatum(this).Print()}>";

    public override string ToString() => $"#<syntax: {ToDatum(this).ToString()}>";

    private static Expr SyntaxPairToDatum(IPair stxPair) {
            Syntax car = stxPair.Car as Syntax ?? throw new Exception($"SyntaxObject.SyntaxPairToDatum: expected syntax pair, but car ({stxPair.Car}) is not a syntax object");
            if (stxPair.Cdr is Expr.NullType) {
                return (Expr)Expr.Pair.Cons(ToDatum(car), (Expr)List.Empty);
            } else if (stxPair.Cdr is Syntax soCdr) {
                return (Expr)Expr.Pair.Cons(ToDatum(car), ToDatum(soCdr));
            } else if (stxPair.Cdr is IPair cdrPair) {
                return (Expr)Expr.Pair.Cons(ToDatum(car), SyntaxPairToDatum(cdrPair));
            } else {
                throw new Exception($"SyntaxPairToDatum: cdr of a syntax pair should be a syntax object, null or a pair. {stxPair.Cdr} is none of these)");
            }
    }

    protected virtual Expr Expression {get;}

    public SrcLoc SrcLoc {get;}
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
            theList = nonEmptyList.CdrAsList;
        }

    }

    IEnumerator IEnumerable.GetEnumerator() {
        return this.GetEnumerator();
    }


}
