using System.Diagnostics.CodeAnalysis;

namespace Jig;

public abstract class ParsedExpr : Syntax {

    protected ParsedExpr(Expr x, SrcLoc? srcLoc = null) : base(x, srcLoc) {}

}

public class IfExpr : ParsedExpr {

    public static bool TryParse(Syntax stx, MacroExpander expander, ExpansionEnvironment ee, [NotNullWhen(returnValue: true)] out IfExpr? ifExpr) {
        SyntaxList? stxList = Syntax.E(stx) as SyntaxList;
        if (stxList is null) {
            ifExpr = null;
            return false;
        }
        List<Syntax> xs = new List<Syntax>();
        if (!Expr.IsKeyword("if", stx)) {
            ifExpr = null;
            return false;
        }
        xs.Add(stxList.ElementAt<Syntax>(0));
        foreach (var x in stxList.Skip<Syntax>(1)) {
            Syntax bodyExpr = expander.Expand(x, ee);
            xs.Add(bodyExpr);
        }
        if (xs.Count == 3) {
            ifExpr = new IfExpr(xs.ElementAt(0), xs.ElementAt(1), xs.ElementAt(2), stx.SrcLoc);
            return true;
        } else if (xs.Count == 4) {
            ifExpr = new IfExpr(xs.ElementAt(0), xs.ElementAt(1), xs.ElementAt(2), xs.ElementAt(3), stx.SrcLoc);
            return true;
        } else {
            throw new Exception($"syntax error: malformed 'if' @{stx.SrcLoc}: {stxList}");
        }
    }

    public Syntax Condition {get; private set;}

    public Syntax Then {get; private set;}

    public Syntax? Else {get; private set;}


    private IfExpr(Syntax kywd, Syntax cond, Syntax then, Syntax @else, SrcLoc? srcLoc = null) :
        base(SyntaxList.FromParams(kywd, cond, then, @else), srcLoc)
    {
        Condition = cond;
        Then = then;
        Else = @else;


    }

    private IfExpr(Syntax kywd, Syntax cond, Syntax then, SrcLoc? srcLoc = null) :
        base(SyntaxList.FromParams(kywd, cond, then), srcLoc)
    {
        Condition = cond;
        Then = then;
    }


}
