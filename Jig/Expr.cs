using System.Collections;
using System.Linq.Expressions;
using System.Reflection;
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

public class Procedure : LiteralExpr<Delegate> {

    public Procedure(Delegate d) : base (d) {}

}

public class ContinuationExpr : Procedure {

    public ContinuationExpr(Delegate d) : base (d) {}

    public void Apply(List args) {
        switch (Value) {
            case ContinuationAny cany:
                cany(args.ToArray());
                return;
            case ListContinuation lc:
                lc(args);
                return;
            case PairContinuation pc:
                pc(args.ElementAt(0),  List.NewList(args.Skip(1).ToArray()));
                return;
            case ImproperListContinuation2 ic2:
                ic2(args.ElementAt(0), args.ElementAt(1), List.NewList(args.Skip(2).ToArray()));
                return;
            case ImproperListContinuation3 ic3:
                ic3(args.ElementAt(0), args.ElementAt(1), args.ElementAt(2), List.NewList(args.Skip(3).ToArray()));
                return;
            case ImproperListContinuation4 ic4:
                ic4(args.ElementAt(0), args.ElementAt(1), args.ElementAt(2), args.ElementAt(3), List.NewList(args.Skip(4).ToArray()));
                return;
            case ImproperListContinuation5 ic5:
                ic5(args.ElementAt(0), args.ElementAt(1), args.ElementAt(2), args.ElementAt(3), args.ElementAt(4), List.NewList(args.Skip(5).ToArray()));
                return;
            case ImproperListContinuation6 ic6:
                ic6(args.ElementAt(0), args.ElementAt(1), args.ElementAt(2), args.ElementAt(3), args.ElementAt(4), args.ElementAt(5), List.NewList(args.Skip(6).ToArray()));
                return;
            case ImproperListContinuation7 ic7:
                ic7(args.ElementAt(0), args.ElementAt(1), args.ElementAt(2), args.ElementAt(3), args.ElementAt(4), args.ElementAt(5), args.ElementAt(6), List.NewList(args.Skip(7).ToArray()));
                return;
            default:
                Value.DynamicInvoke(args.ToArray());
                return;
        }

    }

    public static void call_with_values(Delegate k, LiteralExpr<Delegate> producerExpr, LiteralExpr<Delegate> consumerExpr) {
        var producer = producerExpr.Value; // the producer is a thunk, but internally that is something like (lambda (k) ...)
        var consumer = consumerExpr.Value;
        Delegate cont = ContinuationFromProc(k, consumer);
        Action<Delegate> action = producer as Action<Delegate> ?? throw new Exception("call-with-values: expected first argument to be a thunk.");
        action(cont);
        return;
        // apply(cont, producerExpr, List.Empty);
    }

    private static Delegate ContinuationFromProc(Delegate k, Delegate proc) {
        MethodInfo method = proc.GetType().GetMethod("Invoke") ?? throw new Exception($"ContinuationFromProc: could not find 'Invoke' method on type of proc (proc.GetType())");
        var parameterInfos = method.GetParameters().Skip(1); // get parameters that are not the continuation
        var paramList = new List<ParameterExpression>();
        foreach (var p in parameterInfos)
        {
            paramList.Add(Expression.Parameter(typeof(Expr), p.ToString()));
        }
        // TODO: handle situation when lambda expression has a rest param Eg (lambda (a . rest) rest) or (lambda l l)
        Type? type = GetTypeForContinuation(proc);
        if (type is null) {
            LambdaExpression lexpr = Expression.Lambda(
                body: ET.DynInv(new Expression [] {Expression.Constant(proc), Expression.Constant(k)}.Concat(paramList).ToArray()),
                parameters: paramList.ToArray()
            );
            return lexpr.Compile();
        }
        return Expression.Lambda(
            delegateType: type,
            body: ET.DynInv(new Expression [] {Expression.Constant(proc), Expression.Constant(k)}.Concat(paramList).ToArray()),
            parameters: paramList.ToArray()
        ).Compile();
        throw new NotImplementedException();
    }

    private static Type? GetTypeForContinuation(Delegate proc)
    {
        switch (proc) {

            case ListFunction _:
                return typeof(ListContinuation);
            case PairFunction _:
                return typeof(PairContinuation);
            case ImproperListFunction2 _:
                return typeof(ImproperListContinuation2);
            case ImproperListFunction3 _:
                return typeof(ImproperListContinuation3);
            case ImproperListFunction4 _:
                return typeof(ImproperListContinuation4);
            case ImproperListFunction5 _:
                return typeof(ImproperListContinuation5);
            case ImproperListFunction6 _:
                return typeof(ImproperListContinuation6);
            case ImproperListFunction7 _:
                return typeof(ImproperListContinuation7);
            default:
                return null;
        }
    }


    private delegate void ListContinuation(List rest);
    private delegate void PairContinuation(Expr arg0, List rest);
    private delegate void ImproperListContinuation2(Expr arg0, Expr arg1, List rest);
    private delegate void ImproperListContinuation3(Expr arg0, Expr arg1, Expr arg2, List rest);
    private delegate void ImproperListContinuation4(Expr arg0, Expr arg1, Expr arg2, Expr arg3, List rest);
    private delegate void ImproperListContinuation5(Expr arg0, Expr arg1, Expr arg2, Expr arg3, Expr arg4, List rest);
    private delegate void ImproperListContinuation6(Expr arg0, Expr arg1, Expr arg2, Expr arg3, Expr arg4, Expr arg5, List rest);
    private delegate void ImproperListContinuation7(Expr arg0, Expr arg1, Expr arg2, Expr arg3, Expr arg4, Expr arg5, Expr arg6, List rest);
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
