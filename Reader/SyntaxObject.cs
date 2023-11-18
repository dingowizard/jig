using Jig.Reader;

namespace Jig;

public class SyntaxObject : Expr {

    public SyntaxObject(Expr expr, SrcLoc srcLoc) {
        Datum = expr;
        SrcLoc = srcLoc;
    }

    public static Expr SyntaxE(SyntaxObject so) {
        if (so is SyntaxObject.Pair pair) {
            return (Expr)pair.Syntax;
        } else {
            return so.Datum;
        }
    }

    public class Literal : SyntaxObject {

        public Literal(Expr expr, SrcLoc srcLoc) : base (expr, srcLoc) {}

    }

    public class Identifier : SyntaxObject {
        public Identifier(Expr.Symbol sym, SrcLoc srcLoc) : base (sym, srcLoc) {
            Symbol = sym;
        }

        public new Expr.Symbol Symbol {get;}
    }

    public static Expr ToDatum(IPair pair) {
        SyntaxObject car = pair.Car as SyntaxObject ?? throw new Exception($"ToDatum: car should've been a syntax object, but it was a {pair.Car.GetType()}");
        Expr cdr = pair.Cdr;
        if (cdr is IPair iPair) {
            return (Expr)Expr.Pair.Cons(car.Datum, ToDatum(iPair));
        } else if (cdr is SyntaxObject so) {
            return (Expr)Expr.Pair.Cons(car.Datum, so.Datum);
        } else if (List.IsNull(cdr)) {
                return cdr;
        } else {
            throw new Exception("ToDatum: unexpected condition");
        }
    }


    public new class Pair : SyntaxObject {
        public Pair(IPair expr, SrcLoc srcLoc)
            : base(ToDatum(expr), srcLoc)
        {
            Syntax = expr;

        }

        public IPair Syntax {get;}


    }

    public override string ToString() {
        return $"< SyntaxObject {Datum} >";
    }

    public virtual Expr Datum {get;}

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

    public string Source;
    public int Line;
    public int Column;
    public int Position;
    public int Span;

    internal static SrcLoc Combine(SrcLoc first, SrcLoc last) {
        Console.WriteLine($"first position = {first.Position}, second position = {last.Position}");
        return new SrcLoc(first.Source,
                              first.Line,
                              first.Column,
                              first.Position,
                              (last.Position - first.Position) + last.Span);    }
}
