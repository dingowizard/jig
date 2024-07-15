using System.Collections;
using System.ComponentModel;
using System.Diagnostics.CodeAnalysis;
using System.Reflection.Metadata;
using System.Runtime.CompilerServices;
using System.Text;

namespace Jig;

public abstract class Expr {

    // TODO: decide whether it makes sense to have all of these as nested classes

    public readonly static VoidType Void = new();

    public class VoidType : Expr {
        internal VoidType() {}
        public override string Print() => "#<void>";
    }
    internal class NullType : List {
        public override string Print() => "()";
    }

    public static bool IsNull(Expr x) => x is NullType;

    public class Bool : LiteralExpr<bool> {

        private Bool(bool b) : base(b) {}
        public readonly static Bool True = new (true);
        public readonly static Bool False = new (false);
        public override string Print() => Value ? "#t" : "#f";
    }

    public class Vector : Expr, IEnumerable<Expr> {
        public Vector(params Expr[] xs) {
            Elements = xs;

        }

        public Vector(List xs)
        {
            Elements = [.. xs];
        }

        public bool TryGetAtIndex(Expr.IntegerNumber i, [NotNullWhen(returnValue: true)] out Expr? result) {
            if (i.Value >= Elements.Length) {
                result = null;
                return false;
            } else {
                result = Elements[i.Value];
                return true;
            }

        }

        public Expr.IntegerNumber Length {
            get {
                return new Expr.IntegerNumber(Elements.Length);
            }
        }

        protected Expr[] Elements {get;}

        public override string Print() => $"#({string.Join(' ', this.Select(x => x.Print()))})";

        public IEnumerator<Expr> GetEnumerator() {
            foreach (var x in Elements) {
                yield return x;
            }

        }

        IEnumerator IEnumerable.GetEnumerator() {
            return this.GetEnumerator();
        }

    } // class Vector

    public class Record : Vector {
        public Record(TypeDescriptor rtd, List fields) : base(fields) {
            RecordTypeDescriptor = rtd;
        }
        public Record(TypeDescriptor rtd, Record parent, List fields) : base(fields) {
            RecordTypeDescriptor = rtd;
            Parent = parent;
        }
        protected Record() {
        }

        public TypeDescriptor? RecordTypeDescriptor {get; private set;}

        protected Record? Parent {get;}

        public class TypeDescriptor : Record {

            public TypeDescriptor(List fields) : base(Base, fields) {
                if (fields.Count() != 6) {
                    throw new Exception($"make-record-type-descriptor: expected six arguments, got {fields.Print()}");
                }

                if (fields.ElementAt(0) is not Expr.Symbol name) {
                    throw new Exception("make-record-type-descriptor: expected first argument to be a symbol");
                }
                Name = name;

                if (fields.ElementAt(1) is Expr.Bool b) {
                    if (b == Expr.Bool.True) {
                        throw new Exception($"make-record-type-descriptor: expected second argument to be #f or a record type descriptor. Got: {fields.ElementAt(1)}");
                    }
                } else if (fields.ElementAt(1) is TypeDescriptor parent) {
                    Parent = parent;
                } else {
                    throw new Exception($"make-record-type-descriptor: expected second argument to be #f or a record type descriptor. Got: {fields.ElementAt(1)}");
                }

                if (fields.ElementAt(5) is not Expr.Vector fs) {
                    throw new Exception("in TypeDescriptor cstor: expected second field to be a Vector");
                }
                List<Tuple<Expr.Symbol, bool>> listFields = [];
                foreach (var f in fs) {
                    if (f is not List.NonEmpty listField) {
                        throw new Exception();
                    }
                    if (listField.Count() != 2) {
                        throw new Exception("field spec should have two members");
                    }
                    if (listField.ElementAt(0) is not Expr.Symbol mutability) {
                        throw new Exception("expected a symbol value for field mutability");
                    }
                    if (listField.ElementAt(1) is not Expr.Symbol fieldName) {
                        throw new Exception("expected a symbol value for field mutability");
                    }
                    bool mut = mutability.Equals(new Expr.Symbol("mutable")) || (mutability.Equals(new Expr.Symbol("immutable")) ? false : throw new Exception());
                    listFields.Add(new Tuple<Symbol, bool>(fieldName, mut));
                }
                Fields = [.. listFields];
            }
            public Tuple<Expr.Symbol, bool>[] Fields {get;}
            public new TypeDescriptor? Parent {get;} = null;

            private Thunk? IsOfMe(Delegate k, Record record) {
                if (object.ReferenceEquals(this, record.RecordTypeDescriptor)) {
                    return Continuation.ApplyDelegate(k, Expr.Bool.True);
                } else {
                    if (record.Parent is not null) {
                        return IsOfMe(k, record.Parent);
                    }
                    return Continuation.ApplyDelegate(k, Expr.Bool.False);
                }
            }



            public Procedure Predicate() {
                Builtin predicate = (k, args) => {
                    if (args.Count() != 1) return Builtins.Error(k, $"{this.Name}?: expected exactly one argument but got {args.Count()}");
                    Expr arg = args.ElementAt(0);
                    if (arg is not Record record) {
                        return Continuation.ApplyDelegate(k, Expr.Bool.False);
                    }
                    return IsOfMe(k, record);
                };
                return new Procedure(predicate);

            }

            public Procedure Accessor(Expr.IntegerNumber i) {
                if (i.Value >= Fields.Length) {
                    // a record with two fields has a rtd with three fields (first is name of rtd)
                    // so an index of two would be point to the last field
                    // TODO: the specs for the record fields should probably be in a single field
                    throw new Exception("record-accessor: index out of range");

                }
                Builtin accessor = (k, args) => {
                    // TODO: better error message by getting field name from spec in rtd
                    if (args.Count() != 1) return Builtins.Error(k, $"record access: expected a single argument but got {args.Count()}");
                    var arg = args.ElementAt(0);
                    if (arg is not Record record) {
                        return Builtins.Error(k, $"record access: expected argument to be a record but got {arg}");
                    }
                    if (!object.ReferenceEquals(this, record.RecordTypeDescriptor)) {
                        return Builtins.Error(k, $"record access: expected record of type {this.Name} but got {arg}");
                    }
                    return Continuation.ApplyDelegate(k, record.Elements[i.Value]);
                };
                return new Procedure(accessor);
                

            }

            public Expr.Symbol Name {get;}
            public readonly static TypeDescriptor Base = new BaseType();


            private class BaseType : TypeDescriptor  {
                public BaseType() : base(List.NewList(
                    new Expr.Symbol("base-rtd"),
                    Expr.Bool.False,
                    Expr.Bool.False,
                    Expr.Bool.False,
                    Expr.Bool.False,
                    new Vector()))
                {
                    RecordTypeDescriptor = this;
                }

                public override string Print() => "#!base-rtd";

            }

            public override string Print() => $"#<record type {Name}>";
        }

        public class ConstructorDescriptor : Record {

            public ConstructorDescriptor(List fields) : base(TypeDescriptorForConstructor, fields) {
                if (fields.ElementAt(0) is not Record.TypeDescriptor rtd) {
                    throw new Exception("in ConstructorDescriptor cstor: expected first field to be a record type descriptor");
                }

                RTD = rtd; // note: the ConstructorDescriptor is a record that has two rtds: its own and
                           // a field that contains the RTD for the record it is the constructor of (RTD)
                if (fields.ElementAt(1) is Expr.Bool b) {
                    if (b == Expr.Bool.True) {
                        throw new Exception($"make-record-constructor-descriptor: expected second argument to be #f or a record constructor descriptor. Got: {fields.ElementAt(1)}");
                    }
                } else if (fields.ElementAt(1) is ConstructorDescriptor parent) {
                    Parent = parent;
                } else {
                    throw new Exception($"make-record-constructor-descriptor: expected second argument to be #f or a record constructor descriptor. Got: {fields.ElementAt(1)}");
                }

            }

            private int ParameterCount {
                get
                {
                    int result = RTD.Fields.Length;
                    ConstructorDescriptor? parent = Parent;
                    while (parent is not null)
                    {
                        result += parent.RTD.Fields.Length;
                        parent = parent.Parent;
                    }
                    return result;
                }
            }

            public Procedure Constructor() {
                Builtin constructor = (k, args) => {
                    if (args.Count() != ParameterCount) {
                        return Builtins.Error(k, $"{RTD.Name} constructor: expected {ParameterCount} arguments but got {args.Count()}");
                    }
                    // return Continuation.ApplyDelegate(k, new Record(RTD, List.ListFromEnumerable(args.Skip(1))));
                    if (Parent is null) {
                        return Continuation.ApplyDelegate(k, new Record(RTD, args));
                    } else {
                        int ownArgNum = RTD.Fields.Length;
                        int parentArgNum = ParameterCount - ownArgNum;
                        Record parentRecord = 
                                new Record(
                                    Parent.RTD,
                                    List.ListFromEnumerable(args.Take(parentArgNum)));
                        return Continuation.ApplyDelegate(
                            k,
                            new Record(
                                RTD,
                                parentRecord,
                                List.ListFromEnumerable(args.Skip(parentArgNum))));
                    }
                };
                return new Procedure(constructor);

            }

            public TypeDescriptor RTD {get;}

            public new ConstructorDescriptor? Parent {get;} = null;
            public readonly static TypeDescriptor TypeDescriptorForConstructor =
                new TypeDescriptor(
                    List.NewList(
                        new Expr.Symbol("rcd"),
                        Expr.Bool.False,
                        Expr.Bool.False,
                        Expr.Bool.False,
                        Expr.Bool.False,
                        new Vector()));
        }
    }

    public class Char(char c) : LiteralExpr<char>(c) {
        public override string Print() => $"#\\{Value}";
    }

    public class String(string s) : LiteralExpr<string>(s) {
        public override string Print() {
            // TODO: handle special chars like \n
            return "\"" + Value + "\"";
        }
    }

    public abstract class Number<T>(T item) : Number where T: notnull {
        public T Value { get; } = item;

        public override string Print() => Value.ToString() ?? "";

        public override bool Equals(object? obj) {
            if (obj is null) return false;
            if (obj is Number<T> lit) {
                return this.Value.Equals(lit.Value);
            }
            return false;
        }

        public override int GetHashCode() {
            return Value.GetHashCode();
        }


    }

    public abstract class Number : Expr {

        public static Number From(int i) {
            return new IntegerNumber(i);
        }

        public static Number From(double d) {
            return new DoubleNumber(d);
        }

        public abstract override int GetHashCode();

        public abstract override bool Equals(object? obj);

        public static Expr.Bool operator ==(Number n1, Number n2) {
            return n1 switch
            {
                IntegerNumber in1 => in1 == n2,
                DoubleNumber d1 => d1 == n2,
                _ => throw new NotImplementedException(),
            };
        }

        public static Expr.Bool operator !=(Number n1, Number n2) {
            return n1 switch
            {
                IntegerNumber in1 => in1 != n2,
                DoubleNumber d1 => d1 != n2,
                _ => throw new NotImplementedException(),
            };
        }

        public static Number operator +(Number n1, Number n2) {
            return n1 switch
            {
                IntegerNumber in1 => in1 + n2,
                DoubleNumber d1 => d1 + n2,
                _ => throw new NotImplementedException(),
            };
        }

        public static Number operator -(Number n1, Number n2) {
            return n1 switch
            {
                IntegerNumber in1 => in1 - n2,
                DoubleNumber d1 => d1 - n2,
                _ => throw new NotImplementedException(),
            };
        }

        public static Number operator *(Number n1, Number n2) {
            return n1 switch
            {
                IntegerNumber in1 => in1 * n2,
                DoubleNumber d1 => d1 * n2,
                _ => throw new NotImplementedException(),
            };
        }

        public static Number operator /(Number n1, Number n2) {
            return n1 switch
            {
                IntegerNumber in1 => in1 / n2,
                DoubleNumber d1 => d1 / n2,
                _ => throw new NotImplementedException(),
            };
        }

        public static Expr.Bool operator >(Number n1, Number n2) {
            return n1 switch
            {
                IntegerNumber in1 => in1 > n2,
                DoubleNumber d1 => d1 > n2,
                _ => throw new NotImplementedException(),
            };
        }

        public static Expr.Bool operator <(Number n1, Number n2) {
            return n1 switch
            {
                IntegerNumber in1 => in1 < n2,
                DoubleNumber d1 => d1 < n2,
                _ => throw new NotImplementedException(),
            };
        }
    }

    public class IntegerNumber(int i) : Number<int>(i) {
        public override bool Equals(object? obj) {
            if (obj is null) return false;
            if (obj is IntegerNumber lit) {
                return this.Value.Equals(lit.Value);
            }
            return false;
        }

        public override int GetHashCode() {
            return Value.GetHashCode();
        }

        public static Expr.Bool operator ==(IntegerNumber i1, Number n) {
            return n switch
            {
                IntegerNumber i2 => i1.Value == i2.Value ? Expr.Bool.True : Expr.Bool.False,
                DoubleNumber d2 => i1.Value == d2.Value ? Expr.Bool.True : Expr.Bool.False,
                _ => throw new NotImplementedException(),
            };
        }

        public static Expr.Bool operator !=(IntegerNumber i1, Number n) {
            return n switch
            {
                IntegerNumber i2 => i1.Value != i2.Value ? Expr.Bool.True : Expr.Bool.False,
                DoubleNumber d2 => i1.Value != d2.Value ? Expr.Bool.True : Expr.Bool.False,
                _ => throw new NotImplementedException(),
            };
        }

        public static Number operator +(IntegerNumber i1, Number n) {
            return n switch
            {
                IntegerNumber i2 => new IntegerNumber(i1.Value + i2.Value),
                DoubleNumber d2 => new DoubleNumber(i1.Value + d2.Value),
                _ => throw new NotImplementedException(),
            };
        }

        public static Number operator -(IntegerNumber i1, Number n) {
            return n switch
            {
                IntegerNumber i2 => new IntegerNumber(i1.Value - i2.Value),
                DoubleNumber d2 => new DoubleNumber(i1.Value - d2.Value),
                _ => throw new NotImplementedException(),
            };
        }

        public static Number operator *(IntegerNumber i1, Number n) {
            return n switch
            {
                IntegerNumber i2 => new IntegerNumber(i1.Value * i2.Value),
                DoubleNumber d2 => new DoubleNumber(i1.Value * d2.Value),
                _ => throw new NotImplementedException(),
            };
        }

        public static Number operator /(IntegerNumber i1, Number n) {
            return n switch
            {
                IntegerNumber i2 => new IntegerNumber(i1.Value / i2.Value),
                DoubleNumber d2 => new DoubleNumber(i1.Value / d2.Value),
                _ => throw new NotImplementedException(),
            };
        }

        public static Expr.Bool operator >(IntegerNumber i1, Number n) {
            return n switch
            {
                IntegerNumber i2 => i1.Value > i2.Value ? Expr.Bool.True : Expr.Bool.False,
                DoubleNumber d2 => i1.Value > d2.Value ? Expr.Bool.True : Expr.Bool.False,
                _ => throw new NotImplementedException(),
            };
        }

        public static Expr.Bool operator <(IntegerNumber i1, Number n) {
            return n switch
            {
                IntegerNumber i2 => i1.Value < i2.Value ? Expr.Bool.True : Expr.Bool.False,
                DoubleNumber d2 => i1.Value < d2.Value ? Expr.Bool.True : Expr.Bool.False,
                _ => throw new NotImplementedException(),
            };
        }

    }

    public class DoubleNumber(double d) : Number<double>(d) {
        public override bool Equals(object? obj) {
            if (obj is null) return false;
            if (obj is DoubleNumber lit) {
                return this.Value.Equals(lit.Value);
            }
            return false;
        }

        public override int GetHashCode() {
            return Value.GetHashCode();
        }

        public static Expr.Bool operator ==(DoubleNumber d1, Number n) {
            return n switch
            {
                IntegerNumber i2 => d1.Value == i2.Value ? Expr.Bool.True : Expr.Bool.False,
                DoubleNumber d2 => d1.Value == d2.Value ? Expr.Bool.True : Expr.Bool.False,
                _ => throw new NotImplementedException(),
            };
        }

        public static Expr.Bool operator !=(DoubleNumber d1, Number n) {
            return n switch
            {
                IntegerNumber i2 => d1.Value != i2.Value ? Expr.Bool.True : Expr.Bool.False,
                DoubleNumber d2 => d1.Value != d2.Value ? Expr.Bool.True : Expr.Bool.False,
                _ => throw new NotImplementedException(),
            };
        }
        public static Number operator +(DoubleNumber d1, Number n) {
            // TODO: is it  better to use overrides rather than switch statements?
            return n switch
            {
                IntegerNumber i2 => new DoubleNumber(d1.Value + i2.Value),
                DoubleNumber d2 => new DoubleNumber(d1.Value + d2.Value),
                _ => throw new NotImplementedException(),
            };
        }

        public static Number operator -(DoubleNumber d1, Number n) {
            return n switch
            {
                IntegerNumber i2 => new DoubleNumber(d1.Value - i2.Value),
                DoubleNumber d2 => new DoubleNumber(d1.Value - d2.Value),
                _ => throw new NotImplementedException(),
            };
        }

        public static Number operator *(DoubleNumber d1, Number n) {
            return n switch
            {
                IntegerNumber i2 => new DoubleNumber(d1.Value * i2.Value),
                DoubleNumber d2 => new DoubleNumber(d1.Value * d2.Value),
                _ => throw new NotImplementedException(),
            };
        }

        public static Number operator /(DoubleNumber d1, Number n) {
            return n switch
            {
                IntegerNumber i2 => new DoubleNumber(d1.Value / i2.Value),
                DoubleNumber d2 => new DoubleNumber(d1.Value / d2.Value),
                _ => throw new NotImplementedException(),
            };
        }

        public static Expr.Bool operator >(DoubleNumber d1, Number n) {
            return n switch
            {
                IntegerNumber i2 => d1.Value > i2.Value ? Expr.Bool.True : Expr.Bool.False,
                DoubleNumber d2 => d1.Value > d2.Value ? Expr.Bool.True : Expr.Bool.False,
                _ => throw new NotImplementedException(),
            };
        }

        public static Expr.Bool operator <(DoubleNumber d1, Number n) {
            return n switch
            {
                IntegerNumber i2 => d1.Value < i2.Value ? Expr.Bool.True : Expr.Bool.False,
                DoubleNumber d2 => d1.Value < d2.Value ? Expr.Bool.True : Expr.Bool.False,
                _ => throw new NotImplementedException(),
            };
        }

    }

    public class Symbol : Expr {
        public static Symbol FromName(string name) => name switch {
            "lambda" => new Keyword.Lambda(),
            "if" => new Keyword.If(),
            "define" => new Keyword.Define(),
            "set!" => new Keyword.Set(),
            "quote" => new Keyword.Quote(), // TODO: do they have to be new? couldn't they be static instances on Keyword?
            _ => new Symbol(name),
        };

        public Symbol(string name) {
            Name = name;
        }

        public Symbol(string name, Binding binding) {
            Name = name;
            Binding = binding;
        }

        internal Binding? Binding;

        public virtual string Name {get;}

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
            if (car is Syntax stxCar) {
                if (cdr == List.Empty) {
                    return new SyntaxList(stxCar, List.Empty);
                } else if (cdr is SyntaxList stxListCdr) {
                    return new SyntaxList(stxCar, stxListCdr);
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
                return  new List.NonEmpty(car, list);
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
        Expr x = ast is Syntax stx ? Syntax.E(stx) : ast;
        return x switch
        {
            Expr.Char => true,
            Expr.Bool => true,
            Expr.String => true,
            Expr.IntegerNumber => true,
            Expr.DoubleNumber => true,
            _ => false,
        };
    }

    internal static bool IsSymbol(Expr ast)
    {
        if (ast is Expr.Symbol) return true;
        if (ast is Syntax.Identifier) return true;
        return false;
    }

    internal static bool IsNonEmptyList(Expr ast)
    {
        if (ast is Syntax stx) {
            if (Syntax.E(stx) is List list) {
                return list.Any();
            }
        }
        return ast is List.NonEmpty;
    }

    internal static bool IsNonEmptyList(Expr ast, [NotNullWhen(returnValue: true)]out List.NonEmpty? list)
    {

        if (ast is Syntax stx) {
            if ( Syntax.E(stx) is List.NonEmpty l) {
                list = l;
                return true;
            } else {
                list = null;
                return false;
            }
        }
        if (ast is List.NonEmpty last) {
            list = last;
            return true;

        }
        list = null;
        return false;
    }

    internal static bool IsKeyword(string name, Expr ast) {
        if (ast is Syntax stx) {
            if (Syntax.E(stx) is List list) {
                if (list.ElementAt(0) is Syntax.Identifier id) {
                    return id.Symbol.Name == name;
                } return false;
            } return false;
        }
        if (ast is List.NonEmpty l) {
            return l.Car is Expr.Symbol sym && sym.Name == name;
        } return false;
    }

}

public class SyntaxPair : Expr.Pair {
    public SyntaxPair(Syntax car, Syntax cdr) : base(car,cdr) {}
}

public abstract class Keyword : Expr.Symbol {

    public Keyword(string name) : base (name) {}

    public class Lambda : Keyword {
        public Lambda() : base("lambda") {}
    }

    public class If : Keyword {
        public If() : base("if") {}
    }

    public class Define : Keyword {
        public Define() : base("define") {}
    }

    public class Set : Keyword {
        public Set() : base("set!") {}
    }

    public class Quote : Keyword {
        public Quote() : base("quote") {}
    }
    public static bool Is<T>(Expr car) where T : Keyword => car switch {
            Syntax.Identifier id => id.Symbol is T,
            Expr.Symbol symbol => symbol is T,
            _ => false,
        };
}




public class LiteralExpr<T> : Expr where T : notnull {
    // TODO: make abstract?
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
    string Print();
}

public abstract class List : Expr, IEnumerable<Expr> {

    public static List Empty {get;} = new NullType();

    public static List ListFromEnumerable(IEnumerable<Expr> elements) {
        List result = Empty;
        for (int index = elements.Count() - 1; index >= 0; index--) {
            result = new NonEmpty(elements.ElementAt(index), result);
        }
        return result;
    }

    public static List NewList(params Expr[] args) {
        List result = Empty;
        for (int index = args.Length - 1; index >= 0; index--) {
            result = new NonEmpty(args[index], result);
        }
        return result;
    }

    public Expr Append(Expr x) {
        switch (x) {
            case List.NullType:
                return this;
            case List.NonEmpty properList:
                return ListFromEnumerable(this.Concat(properList));
            default:
                Expr result = x;
                var array = this.ToArray();
                for (int i = array.Length - 1; i>=0; i--) {
                    result = (Expr)Pair.Cons(array[i], result);
                }
                return result;

        }
    }


    public override string ToString() {
        return $"({string.Join(' ', this)})";
    }

    public class NonEmpty : List, IPair {

        public NonEmpty(Expr car, List cdr) {
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

        public Expr Car {get; set;}
        public Expr Cdr {get; set;}

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


    public IEnumerator<Expr> GetEnumerator() {
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
