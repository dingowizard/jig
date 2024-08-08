using System.Text;

namespace Jig;

public class Pair<T, U> :  Pair, IPair<T, U>, IPair where T : Form where U : Form {
    public Pair (T car, U cdr) : base(car,cdr) {
        Car = car;
        Cdr = cdr;
    }

    public new T Car {get;}
    public new U Cdr {get;}


}
