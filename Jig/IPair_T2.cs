namespace Jig;
public interface IPair<T,S> : IPair where T : SchemeValue where S : SchemeValue {
    new T Car {get;}
    new S Cdr {get;}

}