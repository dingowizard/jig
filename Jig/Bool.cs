namespace Jig;

public class Bool : LiteralExpr<bool> {

    private Bool(bool b) : base(b) {}
    public readonly static Bool True = new (true);
    public readonly static Bool False = new (false);
    public override string Print() => Value ? "#t" : "#f";
}