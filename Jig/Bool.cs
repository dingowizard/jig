namespace Jig;

public class Bool : LiteralExpr<bool> {

    private Bool(bool b) : base(b) {}
    public static readonly Bool True = new (true);
    public static readonly Bool False = new (false);
    public override string Print() => Value ? "#t" : "#f";
}