namespace Jig;

public delegate Thunk MacroDelegate(Delegate k, Syntax stx);

public class Transformer : LiteralExpr<Delegate> {
    public Transformer(Delegate del) : base (del) {
        TransformerDelegate = del as Func<Delegate, Expr, Thunk> ?? throw new Exception($"Transfomer must be a lambda that takes a single argment (got {del})");
    }

    public Syntax Apply(Syntax stx) {
        Expr? result = null;
        Continuation.OneArgDelegate setResult = (x) => {result = x; return null;};
        Thunk? thunk = TransformerDelegate(setResult, stx);
        // TODO: this seems crazy
        while (thunk is not null) {
            thunk = thunk();
        }
        if (result is null) throw new Exception($"apply transformer: outupt is null");
        return result as Syntax ?? throw new Exception();

    }

     Func<Delegate, Expr, Thunk> TransformerDelegate {get;}

    public override string Print() => "#<transformer>";
}
