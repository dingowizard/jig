namespace Jig;

public interface IEnvironment {

    Thunk LookUp(Delegate k, Expr symbol);
    Thunk Define(Delegate k, Expr symbol, Expr val);
    Thunk Set(Delegate k, Expr symbol, Expr val);
    IEnumerable<Expr.Symbol> Symbols {get;}
    Expr this[Expr.Symbol symbol] {get;}

}
