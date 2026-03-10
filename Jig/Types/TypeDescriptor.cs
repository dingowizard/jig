namespace Jig.Types;

public class TypeDescriptor {
    
    public virtual Type ClrType {get;}
    
    public virtual Func<SchemeValue, Bool> Predicate {get;}
    
    public virtual Func<SchemeValue, object> ConvertArg {get;}
    
    public virtual Func<object, SchemeValue> WrapReturn {get;}

    public static TypeDescriptor Double => new DoubleTypeDescriptor();
    public static TypeDescriptor Int32 => new Int32TypeDescriptor();

}

internal class Int32TypeDescriptor :  TypeDescriptor {
    
    public override Type ClrType => typeof(int);

    public override Func<SchemeValue, Bool> Predicate {get;} = sv => {
        return sv switch
        {
            Integer => Bool.True,
            _ => Bool.False,
        };
    };
    
    public override Func<SchemeValue, object> ConvertArg {get;} = sv => {
        switch (sv) {
            case Integer z: return z.Value; 
            default: throw new Exception($"Can't convert from {sv.GetType()} to {typeof(double)}");
        }
    };

    public override Func<object, SchemeValue> WrapReturn {get;} = obj => {
        switch (obj) {
            case Int32 z: return new Integer(z);
            case null: throw new Exception($"expected an int, but got null");
            default: throw new Exception($"expected an int, but got {obj.GetType()}");
        }
    };

}

internal class DoubleTypeDescriptor :  TypeDescriptor {
    
    public override Type ClrType => typeof(double);

    public override Func<SchemeValue, Bool> Predicate => _predicate;
    
    public override Func<SchemeValue, object> ConvertArg => _convertArg;

    public override Func<object, SchemeValue> WrapReturn => _wrapReturn;

    private Bool _predicate(SchemeValue sv) {
        switch (sv) {
            case Integer: 
            case Float:
                return Bool.True;
            default:
                return Bool.False;
        }
    }

    private object _convertArg(SchemeValue sv) {
        switch (sv) {
            case Integer z: return (double)z.Value; 
            case Float f: return f.Value;
            default: throw new Exception($"Can't convert from {sv.GetType()} to {typeof(double)}");
        }
    }

    private SchemeValue _wrapReturn(object obj) {
        switch (obj) {
            case Double d: return new Float(d);
            case null: throw new Exception($"expected a double, but got null");
            default: throw new Exception($"expected a double, but got {obj.GetType()}");
        }
    }
}

public class GenericTypeDescriptor : TypeDescriptor {
    
    public TypeDescriptor[] ChildDescriptors {get;}
    
    public Type[] TypeArguments {get;}
    
}