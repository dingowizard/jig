namespace Jig.Types;

public class TypeDescriptor {
    
    public virtual Type ClrType {get;}
    
    public virtual Func<SchemeValue, Bool> Predicate {get;}
    
    public virtual Func<SchemeValue, object> ConvertArg {get;}
    
    public virtual Func<object, SchemeValue> WrapReturn {get;}

    public static TypeDescriptor Double => new DoubleTypeDescriptor();
    public static TypeDescriptor Int32 => new Int32TypeDescriptor();

    public static TypeDescriptor String => new StringTypeDescriptor();

}

public class TypeDescriptor<T, TU> : TypeDescriptor where TU : LiteralExpr<T>, new() where T : notnull {
    public override Type ClrType => typeof(T);
    
    public override Func<SchemeValue, Bool> Predicate {get;} = sv => {
        return sv switch
        {
            TU => Bool.True,
            _ => Bool.False,
        };
    };
    
    public override Func<SchemeValue, object> ConvertArg {get;} = sv => {
        switch (sv) {
            case TU lit: return lit.Value; 
            default: throw new Exception($"Can't convert from {sv.GetType()} to {typeof(T)}");
        }
    };
    
    public override Func<object?, SchemeValue> WrapReturn {get;} = obj => {
        switch (obj) {
            // case T z: return new TU(z);
            case null: return Bool.False;
            default: throw new Exception($"expected a string, but got {obj.GetType()}");
        }
    };
    
}

// TODO: seems like a lot of these could inherit from the same base class that handles simple types that map to Literal<T>
internal class StringTypeDescriptor :  TypeDescriptor {
    
    public override Type ClrType => typeof(string);

    public override Func<SchemeValue, Bool> Predicate {get;} = sv => {
        return sv switch
        {
            Jig.String => Bool.True,
            _ => Bool.False,
        };
    };
    
    public override Func<SchemeValue, object> ConvertArg {get;} = sv => {
        switch (sv) {
            case String str: return str.Value; 
            default: throw new Exception($"Can't convert from {sv.GetType()} to clr string");
        }
    };

    public override Func<object?, SchemeValue> WrapReturn {get;} = obj => {
        switch (obj) {
            case System.String z: return new Jig.String(z);
            case null: return Bool.False;
            default: throw new Exception($"expected a string, but got {obj.GetType()}");
        }
    };
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