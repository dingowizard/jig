namespace Jig;
public interface IPair<T,S> : IPair where T : ISchemeValue where S : ISchemeValue {
    new T Car {get;}
    new S Cdr {get;}

}