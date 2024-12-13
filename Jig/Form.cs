namespace Jig;

public abstract class Form : IForm {

    // TODO: decide whether it makes sense to have all of these as nested classes

    public static readonly VoidType Void = new();

    public class VoidType : LiteralExpr {
        internal VoidType() {}
        public override string Print() => "#<void>";
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
            return obj switch {
                null => false,
                Symbol sym2 => this.Name == sym2.Name,
                _ => false
            };
        }

        public override int GetHashCode() {
            return Name.GetHashCode();
        }

        public override string ToString() {
            return Name;
        }

        public override string Print() => Name;
    }


    public abstract string Print();

    internal static bool IsSymbol(IForm ast)
    {
        switch (ast) {
            case Symbol:
            case Syntax.Identifier:
                return true;
            default:
                return false;
        }
    }

    internal static bool IsNonEmptyList(IForm ast)
    {
        if (ast is Syntax stx) {
            if (Syntax.E(stx) is List list) {
                return list.Any();
            }
        }
        return ast is List.NonEmpty;
    }

    

    internal static bool IsKeyword(string name, IForm ast) {
        switch (ast) {
            case Syntax stx when Syntax.E(stx) is List list: {
                if (list is IEmptyList) {
                    throw new Exception($"IsKeyword: ast is ()");
                }
                if (list.ElementAt(0) is Syntax.Identifier id) {
                    return id.Symbol.Name == name;
                } return false;
            }
            case Syntax:
                return false;
            case List.NonEmpty l:
                return l.Car is Symbol sym && sym.Name == name;
            default:
                return false;
        }
    }

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
            Symbol symbol => symbol is T,
            _ => false,
        };
}