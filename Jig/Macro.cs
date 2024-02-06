namespace Jig;

public class Macro : LiteralExpr<Delegate> {
    public Macro(Delegate del) : base (del) {}

    public Syntax Apply(List args) {
        Procedure proc = new Procedure(Value);
        Syntax result = new Syntax(List.Empty, new SrcLoc()); // TODO: ugh
        Action<Syntax> setResult = (x) => result = x;
        proc.Apply(setResult, args);
        return result;

    }

    public override string Print() => "#<macro>";
}
