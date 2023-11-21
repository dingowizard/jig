
namespace Jig;

public class SyntaxObject : Expr {

    public SyntaxObject(Expr expr, SrcLoc srcLoc) {
        Expression = expr;
        SrcLoc = srcLoc;
    }

    public static SyntaxObject FromDatum(Expr datum, SrcLoc srcLoc) {
        switch (datum) {
            case Expr.Boolean: case Expr.Integer: case Expr.Double:
                return new Literal(datum, srcLoc);
            case Expr.Symbol sym:
                return new Identifier(sym, srcLoc);
            default:
                return new SyntaxObject(datum, srcLoc);
        }
    }

    public class Literal : SyntaxObject {
        internal Literal(Expr x, SrcLoc srcLoc) : base (x, srcLoc) {}

    }

    public class Identifier : SyntaxObject {
        internal Identifier(Expr.Symbol symbol, SrcLoc srcLoc) : base (symbol, srcLoc) {}

        public new Expr.Symbol Symbol {
            get {
                return (Symbol)Expression;
            }
        }
    }

    public override string Print() => $"#<syntax: {ToDatum(this).Print()}>";

    public override string ToString() => $"#<syntax: {ToDatum(this).ToString()}>";

    public static Expr E(SyntaxObject stx) {
        return stx.Expression;
    }

    public static Expr ToDatum(SyntaxObject stx) {
        Expr x = SyntaxObject.E(stx);
        if (x is IPair stxPair) {
            return SyntaxPairToDatum(stxPair);
        }
        return x;
    }

    private static Expr SyntaxPairToDatum(IPair stxPair) {
            SyntaxObject car = stxPair.Car as SyntaxObject ?? throw new Exception($"SyntaxObject.SyntaxPairToDatum: expected syntax pair, but car ({stxPair.Car}) is not a syntax object");
            if (stxPair.Cdr is Expr.NullType) {
                return (Expr)Expr.Pair.Cons(ToDatum(car), (Expr)List.Empty);
            } else if (stxPair.Cdr is SyntaxObject soCdr) {
                return (Expr)Expr.Pair.Cons(ToDatum(car), ToDatum(soCdr));
            } else if (stxPair.Cdr is IPair cdrPair) {
                return (Expr)Expr.Pair.Cons(ToDatum(car), SyntaxPairToDatum(cdrPair));
            } else {
                throw new Exception($"SyntaxPairToDatum: cdr of a syntax pair should be a syntax object, null or a pair. {stxPair.Cdr} is none of these)");
            }
    }



    // public override string ToString() {
    //     return $"< SyntaxObject {Datum} >";
    // }

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
