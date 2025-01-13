namespace Jig;

public abstract class LiteralExpr<T> : LiteralExpr where T : notnull {
    public LiteralExpr(T val) {
        Value = val;
    }
    public T Value {get;}

    public override bool Equals(object? obj) {
        return obj switch {
            null => false,
            LiteralExpr<T> lit => this.Value.Equals(lit.Value),
            _ => false
        };
    }

    public override int GetHashCode() {
        return Value.GetHashCode();
    }

    public override string ToString() => Value.ToString() ?? "null";

    public override string Print() => Value.ToString() ?? "null";

}
