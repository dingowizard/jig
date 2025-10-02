using Jig;
namespace DLR;

public delegate Thunk MacroDelegate(Delegate k, Syntax stx);

public class Transformer : LiteralExpr<Delegate> {
    public Transformer(Delegate del) : base (del) {
        TransformerDelegate = del as Func<Delegate, Form, Thunk?> ?? throw new Exception($"Transfomer must be a lambda that takes a single argment (got {del})");
    }

    public Syntax Apply(Syntax stx) {
        // Console.WriteLine($"\tto {stx} @ {stx.SrcLoc}");
        IForm? result = null;
        Continuation.OneArgDelegate setResult =  Thunk? (x) => {result = x; return null;};
        Thunk? thunk = TransformerDelegate(setResult, stx);
        // TODO: this seems crazy
        while (thunk is not null) {
            thunk = thunk();
        }
        // TODO: if Error is called somewhere in execution of transformer, we get null result
        // TODO: When the assertion fails, stdin is all fucked up
        if (result is null) {throw new Exception($"Transformer.Apply: macro application failed with error");}
        return result as Syntax ?? throw new Exception($"transformer must return a syntax object. (got '{result}')");

    }

     Func<Delegate, Form, Thunk?> TransformerDelegate {get;}

    public override string Print() => "#<transformer>";
}
