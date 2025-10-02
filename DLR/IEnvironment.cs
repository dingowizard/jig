using Jig;

namespace DLR;

public interface IEnvironment {

    Thunk? LookUp(Delegate k, Form symbol);
    Thunk? Define(Delegate k, Form symbol, Form val);
    Thunk? Set(Delegate k, Form symbol, Form val);
    IEnumerable<Symbol> Symbols {get;}
    Form this[Symbol symbol] {get;}

}
