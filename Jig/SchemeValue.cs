namespace Jig;

public abstract class SchemeValue : ISchemeValue {

    // TODO: decide whether it makes sense to have all of these as nested classes

    public static readonly VoidType Void = new();

    public class VoidType : LiteralExpr {
        internal VoidType() {}
        public override string Print() => "#<void>";
    }

    public abstract string Print();

    public static bool IsSymbol(ISchemeValue ast)
    {
        switch (ast) {
            case Symbol:
            case Identifier:
                return true;
            default:
                return false;
        }
    }

    public static bool IsNonEmptyList(ISchemeValue ast)
    {
        if (ast is Syntax stx) {
            if (Syntax.E(stx) is List list) {
                return list.Any();
            }
        }
        return ast is List.NonEmpty;
    }


    public static bool IsKeyword(string name, ISchemeValue ast) {
        switch (ast) {
            case Syntax stx when Syntax.E(stx) is List list: {
                if (list is IEmptyList) {
                    throw new Exception($"IsKeyword: ast is ()");
                }
                if (list.ElementAt(0) is Identifier id) {
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

public abstract class Keyword : Symbol {

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
    public static bool Is<T>(SchemeValue car) where T : Keyword => car switch {
            Identifier id => id.Symbol is T,
            Symbol symbol => symbol is T,
            _ => false,
        };
}