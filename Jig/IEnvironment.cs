namespace Jig;

public interface IEnvironment {

    Thunk? LookUp(Delegate k, Form symbol);
    Thunk? Define(Delegate k, Form symbol, Form val);
    Thunk? Set(Delegate k, Form symbol, Form val);
    IEnumerable<Form.Symbol> Symbols {get;}
    Form this[Form.Symbol symbol] {get;}

}
