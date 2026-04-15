using Jig.Types;
namespace Jig;

public class LiteralExpr<T> : LiteralExpr, IHasTypeDescriptor where T : notnull {
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

    public override string Print() => $"#<{Value.GetType().Name} {Value.ToString()}>" ?? "null";

    public static  TypeDescriptor TypeDescriptor {get;} = new SchemeValueTypeDescriptor<LiteralExpr<T>>();
}

public interface IHasTypeDescriptor {
    abstract static TypeDescriptor TypeDescriptor { get; }
}
