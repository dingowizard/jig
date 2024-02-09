namespace Jig;

public delegate Thunk MacroDelegate(Delegate k, Syntax stx);

public class Macro : LiteralExpr<MacroDelegate> {
    public Macro(MacroDelegate del) : base (del) {}

    public Syntax Apply(Syntax stx) {
        Syntax result = new Syntax(List.Empty, new SrcLoc()); // TODO: ugh
        Action<Syntax> setResult = (x) => result = x;
        Value(setResult, stx);
        return result;

    }

    public override string Print() => "#<macro>";
}
