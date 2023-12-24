namespace Jig;

public interface IEnvironment {

    void LookUp(Delegate k, Expr symbol);
    void Define(Delegate k, Expr symbol, Expr val);
    void Set(Delegate k, Expr symbol, Expr val);

}
