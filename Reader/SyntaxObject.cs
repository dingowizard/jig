namespace Jig;

public class SyntaxObject {

    public SyntaxObject(Expr expr, SrcLoc srcLoc) {
        Datum = expr;
        SrcLoc = srcLoc;
    }

    public class Literal : SyntaxObject {

        public Literal(Expr expr, SrcLoc srcLoc) : base (expr, srcLoc) {}

    }

    public class Identifier : SyntaxObject {
        public Identifier(Expr.Symbol sym, SrcLoc srcLoc) : base (sym, srcLoc) {
            Symbol = sym;
        }

        public Expr.Symbol Symbol {get;}
    }

    public class Pair : SyntaxObject {
        public Pair(Expr car, Expr cdr, SrcLoc srcLoc) : base( (Expr)Expr.Pair.Cons(car, cdr), srcLoc) {}

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
    public string Source;
    public int Line;
    public int Column;
    public int Position;
    public int Span;

}
