namespace Jig;

public class Macro : LiteralExpr<Delegate> {
    public Macro(Delegate del) : base (del) {}

    public Expr Apply(List args) {
        Procedure proc = new Procedure(Value);
        Expr result = List.Empty;
        Action<Expr> setResult = (x) => result = x;
        proc.Apply(setResult, args);
        return result;

    }

    public override string Print() => "#<macro>";
}
