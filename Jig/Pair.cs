using System.Text;

namespace Jig;

public class Pair : SchemeValue, IPair {

    public static IPair Cons(SchemeValue car, SchemeValue cdr) {
        if (car is Syntax stxCar) {
            if (cdr is List { IsEmpty: true }) {
                return SyntaxList.Cons(stxCar, SyntaxList.Null);
            } else if (cdr is SyntaxList stxListCdr) {
                return SyntaxList.Cons(stxCar, stxListCdr);
            } else if (cdr is Syntax stxCdr) {
                return new SyntaxPair(stxCar, stxCdr);
            } else {
                if (cdr is List l) {
                    return  new List.NonEmpty(car, l);
                } else {
                    return  new Pair(car, cdr);
                }
            }
        }
        if (cdr is List list) {
            return new List.NonEmpty(car, list);
        } else {
            return  new Pair(car, cdr);
        }
    }

    protected Pair(SchemeValue car, SchemeValue cdr) {
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