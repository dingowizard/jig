namespace Jig;

public abstract class LiteralExpr<T> : LiteralExpr where T : notnull {
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
