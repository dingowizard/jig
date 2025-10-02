using Jig;

namespace DLR;

public interface IEnvironment {

    Thunk? LookUp(Delegate k, SchemeValue symbol);
    Thunk? Define(Delegate k, SchemeValue symbol, SchemeValue val);
    Thunk? Set(Delegate k, SchemeValue symbol, SchemeValue val);
    IEnumerable<Symbol> Symbols {get;}
    SchemeValue this[Symbol symbol] {get;}

}
