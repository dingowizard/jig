namespace Jig;

public interface IEnvironment {

    Thunk LookUp(Delegate k, Expr symbol);
    Thunk Define(Delegate k, Expr symbol, Expr val);
    Thunk Set(Delegate k, Expr symbol, Expr val);

}
