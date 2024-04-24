namespace Jig;

public abstract class ParsedExpr : Syntax {

    public ParsedExpr(Expr x, SrcLoc? srcLoc = null) : base(x, srcLoc) {}

}

public class IfExpr : ParsedExpr {

    public IfExpr(Expr x, SrcLoc? srcLoc = null) : base (x, srcLoc) {}

}
