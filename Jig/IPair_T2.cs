namespace Jig;
public interface IPair<T,S> : IPair where T : IForm where S : IForm {
    new T Car {get;}
    new S Cdr {get;}

}