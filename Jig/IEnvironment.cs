namespace Jig;

public interface IEnvironment {

    Continuation.MaybeThunk LookUp(Delegate k, Expr symbol);
    Continuation.MaybeThunk Define(Delegate k, Expr symbol, Expr val);
    void Set(Delegate k, Expr symbol, Expr val);

}
