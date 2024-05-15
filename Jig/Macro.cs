using System.Diagnostics;

namespace Jig;

public delegate Thunk MacroDelegate(Delegate k, Syntax stx);

public class Transformer : LiteralExpr<Delegate> {
    public Transformer(Delegate del) : base (del) {
        TransformerDelegate = del as Func<Delegate, Expr, Thunk> ?? throw new Exception($"Transfomer must be a lambda that takes a single argment (got {del})");
    }

    public Syntax Apply(Syntax stx) {
        // Console.WriteLine($"\tto {stx} @ {stx.SrcLoc}");
        Expr? result = null;
        // TODO: someday figure out Thunk and Thunk? and null. Until then, ignore warnings!
        #pragma warning disable CS8603
        Continuation.OneArgDelegate setResult =  Thunk (Expr x) => {result = x; return null;};
        #pragma warning restore CS8603
        Thunk? thunk = TransformerDelegate(setResult, stx);
        // TODO: this seems crazy
        while (thunk is not null) {
            thunk = thunk();
        }
        Debug.Assert(result is not null);
        return result as Syntax ?? throw new Exception($"transformer must return a syntax object. (got '{result}')");

    }

     Func<Delegate, Expr, Thunk> TransformerDelegate {get;}

    public override string Print() => "#<transformer>";
}
