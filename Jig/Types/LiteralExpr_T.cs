using System.Diagnostics;
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

    public static  TypeDescriptor TypeDescriptor {get;} = new LiteralExprTypeDescriptor<T>();
}

public class LiteralExprTypeDescriptor<T> : TypeDescriptor where T : notnull {

    public LiteralExprTypeDescriptor() {
        ClrType = typeof(T);
        Predicate = (sv) => {
            return sv is LiteralExpr<T> ? Bool.True : Bool.False;
        };
        ConvertArg = (sv) => {
            LiteralExpr<T>? litX = sv as LiteralExpr<T>;
            Debug.Assert(litX != null);
            
            var result = litX.Value;
            // Console.Write($"-- Value is {result.GetHashCode()} -- ");
            return result;
        };
        WrapReturn = (obj) => new LiteralExpr<T>((T)obj);
    }

}

public interface IHasTypeDescriptor {
    abstract static TypeDescriptor TypeDescriptor { get; }
}
