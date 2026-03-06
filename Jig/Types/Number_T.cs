namespace Jig;

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