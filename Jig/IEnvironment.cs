namespace Jig;

public interface IEnvironment {

    void LookUp(Continuation k, Expr symbol);
    void Define(Continuation k, Expr symbol, Expr val);
    void Set(Continuation k, Expr symbol, Expr val);

}
