using System.Collections;

namespace Jig;

public abstract class List : Form, IEnumerable<Form> {

    public static List Empty {get;} = new NullType();

    public static List ListFromEnumerable(IEnumerable<Form> elements) {
        List result = Empty;
        for (int index = elements.Count() - 1; index >= 0; index--) {
            result = new NonEmpty(elements.ElementAt(index), result);
        }
        return result;
    }

    public static List NewList(params Form[] args) {
        List result = Empty;
        for (int index = args.Length - 1; index >= 0; index--) {
            result = new NonEmpty(args[index], result);
        }
        return result;
    }

    public Form Append(Form x) {
        switch (x) {
            case List.NullType:
                return this;
            case List.NonEmpty properList:
                return ListFromEnumerable(this.Concat(properList));
            default:
                Form result = x;
                var array = this.ToArray();
                for (int i = array.Length - 1; i>=0; i--) {
                    result = (Form)Pair.Cons(array[i], result);
                }
                return result;

        }
    }


    public override string ToString() {
        return $"({string.Join(' ', this)})";
    }

    public class NonEmpty : List, IPair {

        public NonEmpty(Form car, List cdr) {
            Car = car;
            Cdr = cdr;
            Rest = cdr;
        }

        public override bool Equals(object? obj) {
            if (obj is null) return false;
            if (obj is NonEmpty list) {
                return this.Car.Equals(list.Car) && this.Cdr.Equals(list.Cdr);
            }
            return false;
        }

        public Form Car {get; set;}
        public Form Cdr {get; set;}

        public List Rest {get;}
        public override int GetHashCode() {
            return base.GetHashCode();
        }

        public override string ToString() {
            return Print();
        }

        public override string Print() {
            return "(" + string.Join(" ", this.Select(el => el.Print())) + ")";
        }

    }


    public IEnumerator<Form> GetEnumerator() {
        List theList = this;
        while (theList is NonEmpty nonEmptyList) {
            yield return nonEmptyList.Car;
            theList = nonEmptyList.Rest;
        }

    }

    IEnumerator IEnumerable.GetEnumerator() {
        return this.GetEnumerator();
    }

    public override int GetHashCode() {
        int hash = 19;
        unchecked {
            foreach (var expr in this) {
                hash = hash * 31 + expr.GetHashCode();

            }
        }
        return hash;
    }
}
