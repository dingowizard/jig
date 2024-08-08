namespace Jig;
public interface IPair<T,S> where T : Form where S : Form {
    T Car {get;}
    S Cdr {get;}

}