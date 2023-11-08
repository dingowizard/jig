using System.Collections;

namespace Jig;

public abstract class Expr {

    internal class NullType : List {}

    public static bool IsNull(Expr x) => x is NullType;

    public class Boolean : LiteralExpr {
        public Boolean(bool b) : base(b) {}
    }

    public class Symbol : Expr {
        public Symbol(string name) {
            Name = name;
        }

        public string Name {get;}

        public override bool Equals(object? obj) {
            if (obj is null) return false;
            if (obj is Expr.Symbol sym2) {
                return this.Name == sym2.Name;
            }
            return false;

        }

        public override int GetHashCode() {
            return Name.GetHashCode();
        }

        public override string ToString() {
            return Name;
        }
    }

    public class Pair : Expr, IPair {

        public static IPair Cons(Expr car, List cdr) {
            return new List.NonEmptyList(car, cdr);
        }

        public static IPair Cons(Expr car, Expr cdr) {
            return new Pair(car, cdr);
        }

        protected Pair(Expr car, Expr cdr) {
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

        public Expr Car {get; set;}
        public Expr Cdr {get; set;}

    }

}

public class LiteralExpr : Expr {
    public LiteralExpr(object val) {
        Value = val;
    }
    public object Value {get;}
}

public interface IPair {
    Expr Car {get; set;}
    Expr Cdr {get; set;}
}

public abstract class List : Expr, IEnumerable<Expr> {

    public static List Empty {get;} = new NullType();

    public static List ListFromEnumerable(IEnumerable<Expr> elements) {
        List result = Empty;
        for (int index = elements.Count() - 1; index >= 0; index--) {
            result = new NonEmptyList(elements.ElementAt(index), result);
        }
        return result;
    }

    public static List NewList(params Expr[] args) {
        List result = Empty;
        for (int index = args.Length - 1; index >= 0; index--) {
            result = new NonEmptyList(args[index], result);
        }
        return result;
    }

    public override string ToString() {
        return $"({string.Join(' ', this)})";
    }

    public class NonEmptyList : List, IPair {

        public NonEmptyList(Expr car, List cdr) {
            Car = car;
            Cdr = cdr;
        }

        public override bool Equals(object? obj) {
            if (obj is null) return false;
            if (obj is NonEmptyList list) {
                return this.Car.Equals(list.Car) && this.Cdr.Equals(list.Cdr);
            }
            return false;
        }

        public Expr Car {get; set;}
        public Expr Cdr {get; set;}
        public List CdrAsList {
            get {
                #pragma warning disable CS8603
                return this.Cdr as List;
                #pragma warning disable CS8603
            }

        }

    }

    public IEnumerator<Expr> GetEnumerator() {
        List theList = this;
        while (theList is NonEmptyList nonEmptyList) {
            yield return nonEmptyList.Car;
            theList = nonEmptyList.CdrAsList;
        }

    }

    IEnumerator IEnumerable.GetEnumerator() {
        return this.GetEnumerator();
    }
}
