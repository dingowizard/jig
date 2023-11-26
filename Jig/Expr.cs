using System.Collections;
using System.Text;

namespace Jig;

public abstract class Expr {

    internal class NullType : List {
        public override string Print() => "()";
    }

    public static bool IsNull(Expr x) => x is NullType;

    public class Boolean : LiteralExpr<bool> {
        public Boolean(bool b) : base(b) {}
        public override string Print() => Value ? "#t" : "#f";
    }

    public class BuiltinProc : LiteralExpr<Builtin> {
        public BuiltinProc(Builtin proc) : base(proc) {}
        public override string Print() => "#<Builint proc>";

    }

    public class Integer : LiteralExpr<int> {
        public Integer(int i) : base(i) {}

        public static Double operator +(Integer i, Double d) => new Double(i.Value + d.Value);

        public static Integer operator +(Integer i1, Integer i2) => new Integer(i1.Value + i2.Value);

        public static Double operator -(Integer i, Double d) => new Double(i.Value - d.Value);

        public static Integer operator -(Integer i1, Integer i2) => new Integer(i1.Value - i2.Value);

        public static Double operator *(Integer i, Double d) => new Double(i.Value * d.Value);

        public static Integer operator *(Integer i1, Integer i2) => new Integer(i1.Value * i2.Value);

        public static Double operator /(Integer i, Double d) => new Double(i.Value / d.Value);

        public static Integer operator /(Integer i1, Integer i2) => new Integer(i1.Value / i2.Value);
    }

    public class Double : LiteralExpr<double> {
        public Double(double d) : base(d) {}

        public static Double operator+(Double d1, Double d2) => new Double(d1.Value + d2.Value);

        public static Double operator+(Double d, Expr.Integer i) => new Double(d.Value + i.Value);

        public static Double operator-(Double d1, Double d2) => new Double(d1.Value - d2.Value);

        public static Double operator-(Double d, Expr.Integer i) => new Double(d.Value - i.Value);

        public static Double operator*(Double d1, Double d2) => new Double(d1.Value * d2.Value);

        public static Double operator*(Double d, Expr.Integer i) => new Double(d.Value * i.Value);

        public static Double operator/(Double d1, Double d2) => new Double(d1.Value / d2.Value);

        public static Double operator/(Double d, Expr.Integer i) => new Double(d.Value / i.Value);

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

        public override string Print() => Name;
    }

    public class Pair : Expr, IPair {

        public static IPair Cons(Expr car, Expr cdr) {
            if (cdr is List list) {
                return  new List.NonEmptyList(car, list);
            } else {
                return  new Pair(car, cdr);
            }
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
        public override int GetHashCode() {
            int hash = Car.GetHashCode();
            unchecked {

                hash = hash * 31 + Cdr.GetHashCode();
            }

            return hash;
        }


        public Expr Car {get; set;}
        public Expr Cdr {get; set;}

        public override string Print() {
            StringBuilder sb = new StringBuilder("(");
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

    public abstract string Print();

    internal static bool IsLiteral(Expr ast)
    {
        Expr x = ast is SyntaxObject stx ? SyntaxObject.ToDatum(stx) : ast;
        switch (x) {
            case Expr.Boolean: return true;
            case Expr.Integer: return true;
            case Expr.Double: return true;
            default: return false;
        }

    }

    internal static bool IsSymbol(Expr ast)
    {
        if (ast is Expr.Symbol) return true;
        if (ast is SyntaxObject.Identifier) return true;
        return false;
    }

    internal static bool IsNonEmptyList(Expr ast)
    {
        if (ast is SyntaxObject stx) {
            return SyntaxObject.E(stx) is List.NonEmptyList;
        }
        return ast is List.NonEmptyList;
    }

    internal static bool IsKeyword(string name, Expr ast) {
        if (ast is SyntaxObject stx) {
            if (SyntaxObject.E(stx) is List.NonEmptyList list) {
                if (list.Car is SyntaxObject.Identifier id) {
                    return id.Symbol.Name == name;
                } return false;
            } return false;
        }
        if (ast is List.NonEmptyList l) {
            return l.Car is Expr.Symbol sym && sym.Name == name;
        } return false;
    }

}

public class LiteralExpr<T> : Expr where T : notnull {
    public LiteralExpr(T val) {
        Value = val;
    }
    public T Value {get;}
    public override bool Equals(object? obj) {
        if (obj is null) return false;
        if (obj is LiteralExpr<T> lit) {
            return this.Value.Equals(lit.Value);
        }
        return false;

    }
    public override int GetHashCode() {
        return Value.GetHashCode();
    }
    public override string ToString() => Value.ToString() ?? "null";
    public override string Print() => Value.ToString() ?? "null";
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

    public static List NewListFromObjects(params CompiledCode[] args) {
        List result = Empty;
        for (int index = args.Length - 1; index >= 0; index--) {
            result = new NonEmptyList(new LiteralExpr<CompiledCode>(args[index]), result);
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
