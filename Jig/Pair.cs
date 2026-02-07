using System.Text;

namespace Jig;

public class Pair : SchemeValue, IPair {

    public static IPair Cons(SchemeValue car, SchemeValue cdr) => cdr.Prepend(car);

    internal Pair(SchemeValue car, SchemeValue cdr) {
        Car = car;
        Cdr = cdr;
    }

    public override bool Equals(object? obj) {
        if (obj is null) return false;
        if (obj is Pair p) {
            return p.Car.Equals(this.Car) && p.Cdr.Equals(this.Cdr);
        }
        return false;
    }
    public override int GetHashCode() {
        int hash = Car.GetHashCode();
        unchecked {

            hash = hash * 31 + Cdr.GetHashCode();
        }

        return hash;
    }


    public SchemeValue Car {get; set;}
    public SchemeValue Cdr {get; set;}

    public override string Print() {
        StringBuilder sb = new("(");
        Pair pair = this;
        sb.Append(pair.Car.Print());
        while (pair.Cdr is Pair cdr) {
            pair = cdr;
            sb.Append(" " + pair.Car.Print());
        }
        sb.Append(" . " + pair.Cdr.Print() + ")");
        return sb.ToString();

    }

}