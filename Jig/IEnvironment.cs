namespace Jig;

public interface IEnvironment {

    Continuation.MaybeThunk LookUp(Delegate k, Expr symbol);
    Continuation.MaybeThunk Define(Delegate k, Expr symbol, Expr val);
    Continuation.MaybeThunk Set(Delegate k, Expr symbol, Expr val);

}
