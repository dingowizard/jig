namespace Jig;

public class Char(char c) : LiteralExpr<char>(c) {
    public override string Print() => $"#\\{Value}";
}

