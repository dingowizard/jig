using System.Collections;
using System.ComponentModel;
using System.Diagnostics.CodeAnalysis;
using System.Reflection.Metadata;
using System.Runtime.CompilerServices;
using System.Text;

namespace Jig;

public abstract partial class Form {

    // TODO: decide whether it makes sense to have all of these as nested classes

    public readonly static VoidType Void = new();

    public class VoidType : Form {
        internal VoidType() {}
        public override string Print() => "#<void>";
    }
    internal class NullType : List {
        public override string Print() => "()";
    }

    public static bool IsNull(Form x) => x is NullType;

    public class Bool : LiteralExpr<bool> {

        private Bool(bool b) : base(b) {}
        public readonly static Bool True = new (true);
        public readonly static Bool False = new (false);
        public override string Print() => Value ? "#t" : "#f";
    }

    public class Vector : Form, IEnumerable<Form> {
        public Vector(params Form[] xs) {
            Elements = xs;

        }

        public Vector(List xs)
        {
            Elements = [.. xs];
        }

        public bool TryGetAtIndex(Form.IntegerNumber i, [NotNullWhen(returnValue: true)] out Form? result) {
            if (i.Value >= Elements.Length) {
                result = null;
                return false;
            } else {
                result = Elements[i.Value];
                return true;
            }

        }

        public Form.IntegerNumber Length {
            get {
                return new Form.IntegerNumber(Elements.Length);
            }
        }

        protected Form[] Elements {get;}

        public override string Print() => $"#({string.Join(' ', this.Select(x => x.Print()))})";

        public IEnumerator<Form> GetEnumerator() {
            foreach (var x in Elements) {
                yield return x;
            }

        }

        IEnumerator IEnumerable.GetEnumerator() {
            return this.GetEnumerator();
        }

    } // class Vector

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

    public abstract class Number : Form {

        public static Number From(int i) {
            return new IntegerNumber(i);
        }

        public static Number From(double d) {
            return new DoubleNumber(d);
        }

        public abstract override int GetHashCode();

        public abstract override bool Equals(object? obj);

        public static Form.Bool operator ==(Number n1, Number n2) {
            return n1 switch
            {
                IntegerNumber in1 => in1 == n2,
                DoubleNumber d1 => d1 == n2,
                _ => throw new NotImplementedException(),
            };
        }

        public static Form.Bool operator !=(Number n1, Number n2) {
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

        public static Form.Bool operator >(Number n1, Number n2) {
            return n1 switch
            {
                IntegerNumber in1 => in1 > n2,
                DoubleNumber d1 => d1 > n2,
                _ => throw new NotImplementedException(),
            };
        }

        public static Form.Bool operator <(Number n1, Number n2) {
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

        public static Form.Bool operator ==(IntegerNumber i1, Number n) {
            return n switch
            {
                IntegerNumber i2 => i1.Value == i2.Value ? Form.Bool.True : Form.Bool.False,
                DoubleNumber d2 => i1.Value == d2.Value ? Form.Bool.True : Form.Bool.False,
                _ => throw new NotImplementedException(),
            };
        }

        public static Form.Bool operator !=(IntegerNumber i1, Number n) {
            return n switch
            {
                IntegerNumber i2 => i1.Value != i2.Value ? Form.Bool.True : Form.Bool.False,
                DoubleNumber d2 => i1.Value != d2.Value ? Form.Bool.True : Form.Bool.False,
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

        public static Form.Bool operator >(IntegerNumber i1, Number n) {
            return n switch
            {
                IntegerNumber i2 => i1.Value > i2.Value ? Form.Bool.True : Form.Bool.False,
                DoubleNumber d2 => i1.Value > d2.Value ? Form.Bool.True : Form.Bool.False,
                _ => throw new NotImplementedException(),
            };
        }

        public static Form.Bool operator <(IntegerNumber i1, Number n) {
            return n switch
            {
                IntegerNumber i2 => i1.Value < i2.Value ? Form.Bool.True : Form.Bool.False,
                DoubleNumber d2 => i1.Value < d2.Value ? Form.Bool.True : Form.Bool.False,
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

        public static Form.Bool operator ==(DoubleNumber d1, Number n) {
            return n switch
            {
                IntegerNumber i2 => d1.Value == i2.Value ? Form.Bool.True : Form.Bool.False,
                DoubleNumber d2 => d1.Value == d2.Value ? Form.Bool.True : Form.Bool.False,
                _ => throw new NotImplementedException(),
            };
        }

        public static Form.Bool operator !=(DoubleNumber d1, Number n) {
            return n switch
            {
                IntegerNumber i2 => d1.Value != i2.Value ? Form.Bool.True : Form.Bool.False,
                DoubleNumber d2 => d1.Value != d2.Value ? Form.Bool.True : Form.Bool.False,
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

        public static Form.Bool operator >(DoubleNumber d1, Number n) {
            return n switch
            {
                IntegerNumber i2 => d1.Value > i2.Value ? Form.Bool.True : Form.Bool.False,
                DoubleNumber d2 => d1.Value > d2.Value ? Form.Bool.True : Form.Bool.False,
                _ => throw new NotImplementedException(),
            };
        }

        public static Form.Bool operator <(DoubleNumber d1, Number n) {
            return n switch
            {
                IntegerNumber i2 => d1.Value < i2.Value ? Form.Bool.True : Form.Bool.False,
                DoubleNumber d2 => d1.Value < d2.Value ? Form.Bool.True : Form.Bool.False,
                _ => throw new NotImplementedException(),
            };
        }

    }

    public class Symbol : Form {
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
            if (obj is Form.Symbol sym2) {
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

    public class Pair : Form, IPair {

        public static IPair Cons(Form car, Form cdr) {
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

        protected Pair(Form car, Form cdr) {
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


        public Form Car {get; set;}
        public Form Cdr {get; set;}

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

    internal static bool IsLiteral(Form ast)
    {
        Form x = ast is Syntax stx ? Syntax.E(stx) : ast;
        return x switch
        {
            Form.Char => true,
            Form.Bool => true,
            Form.String => true,
            Form.IntegerNumber => true,
            Form.DoubleNumber => true,
            _ => false,
        };
    }

    internal static bool IsSymbol(Form ast)
    {
        if (ast is Form.Symbol) return true;
        if (ast is Syntax.Identifier) return true;
        return false;
    }

    internal static bool IsNonEmptyList(Form ast)
    {
        if (ast is Syntax stx) {
            if (Syntax.E(stx) is List list) {
                return list.Any();
            }
        }
        return ast is List.NonEmpty;
    }

    internal static bool IsNonEmptyList(Form ast, [NotNullWhen(returnValue: true)]out List.NonEmpty? list)
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

    internal static bool IsKeyword(string name, Form ast) {
        if (ast is Syntax stx) {
            if (Syntax.E(stx) is List list) {
                if (list.ElementAt(0) is Syntax.Identifier id) {
                    return id.Symbol.Name == name;
                } return false;
            } return false;
        }
        if (ast is List.NonEmpty l) {
            return l.Car is Form.Symbol sym && sym.Name == name;
        } return false;
    }

}

public class SyntaxPair : Form.Pair {
    public SyntaxPair(Syntax car, Syntax cdr) : base(car,cdr) {
        Car = car;
        Cdr = cdr;

    }

    public new Syntax Car {get;}
    public new Syntax Cdr {get;}
}

public abstract class Keyword : Form.Symbol {

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
    public static bool Is<T>(Form car) where T : Keyword => car switch {
            Syntax.Identifier id => id.Symbol is T,
            Form.Symbol symbol => symbol is T,
            _ => false,
        };
}




public class LiteralExpr<T> : Form where T : notnull {
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
    Form Car {get; set;}
    Form Cdr {get; set;}
    string Print();
}
