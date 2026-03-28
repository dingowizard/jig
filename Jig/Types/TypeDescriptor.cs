namespace Jig.Types;

public class TypeDescriptor {
    
    public virtual Type ClrType {get;}
    
    public virtual Func<SchemeValue, Bool> Predicate {get;}
    
    public virtual Func<SchemeValue, object> ConvertArg {get;}
    
    // TODO: why are these properties rather than methods?
    public virtual Func<object, SchemeValue> WrapReturn {get;}

    public static TypeDescriptor Double => new DoubleTypeDescriptor();
    public static TypeDescriptor Int32 => new Int32TypeDescriptor();

    public static TypeDescriptor String => new StringTypeDescriptor();

    public static TypeDescriptor Boolean => new BoolTypeDescriptor();

    public static TypeDescriptor Char => new TypeDescriptor<char, Jig.Char>(ch => new Char(ch));

    public static TypeDescriptor SchemeValue => new SchemeValueTypeDescriptor();
    
    public static TypeDescriptor ArrayString => new ArrayStringTypeDescriptor();



}


public class TypeDescriptor<T, TU> : TypeDescriptor where TU : LiteralExpr<T> where T : notnull {
    public override Type ClrType => typeof(T);

    public TypeDescriptor(Func<T, TU> func) {
        _cstor = func;
        WrapReturn = _wrap;
    }
    
    private Func<T, TU> _cstor;
    
    
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

    public override Func<object?, SchemeValue> WrapReturn {get;} 

    private SchemeValue _wrap(object? obj) {
        switch (obj) {
            case null: return Bool.False;
            case T val: return _cstor(val);
            default: throw new Exception($"expected a {ClrType.Name}, but got {obj.GetType()}");
        }
    }
    
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

internal class BoolTypeDescriptor :  TypeDescriptor {
    
    public override Type ClrType => typeof(bool);

    public override Func<SchemeValue, Bool> Predicate {get;} = sv => {
        return sv switch
        {
            Jig.Bool => Bool.True,
            _ => Bool.False,
        };
    };
    
    public override Func<SchemeValue, object> ConvertArg {get;} = sv => {
        switch (sv) {
            case Bool b: return b.Value; 
            default: throw new Exception($"Can't convert from {sv.GetType()} to clr bool");
        }
    };

    public override Func<object?, SchemeValue> WrapReturn {get;} = obj => {
        switch (obj) {
            case bool b: return b ? Bool.True : Bool.False;
            case null: return Bool.False;
            default: throw new Exception($"expected a bool, but got {obj.GetType()}");
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

public class SchemeValueTypeDescriptor : TypeDescriptor {
    
    
    public override Type ClrType => typeof(SchemeValue);

    public override Func<SchemeValue, Bool> Predicate => _predicate;
    
    public override Func<SchemeValue, object> ConvertArg => _convertArg;

    public override Func<object, SchemeValue> WrapReturn => _wrapReturn;

    private Bool _predicate(SchemeValue sv) { return Bool.True; }

    private object _convertArg(SchemeValue sv) {
        return sv;
    }

    private SchemeValue _wrapReturn(object obj) {
        switch (obj) {
            case Jig.SchemeValue sv: return sv;
            case null: throw new Exception($"expected a SchemeValue, but got null");
            default: throw new Exception($"expected a SchemeValue, but got {obj.GetType()}");
        }
    }
}


public class ArrayStringTypeDescriptor : TypeDescriptor {
    
    // TODO: this should be built out of the Generic Type Descriptor and the TypeDescriptor for string. We'll get there
    
    
    public override Type ClrType => typeof(string[]);

    private Bool _predicate(SchemeValue sv) {
        if (sv is Vector vector) {
            foreach (var v in vector.Elements) {
                if (!(v is String)) {
                    return Bool.False;
                }
            }
            return Bool.True;
        }
        return Bool.False;
    }

    private object _convertArg(SchemeValue sv) {
        return ((Vector)sv).Elements.Select(x => ((String)x).Value).ToArray();
    }

    private SchemeValue _wrapReturn(object obj) {
        switch (obj) {
            case string[] arr: return new Vector(arr.Select(str => new String(str)));
            case null: throw new Exception($"expected a string[], but got null");
            default: throw new Exception($"expected a string[], but got {obj.GetType()}");
        }
    }
    public override Func<SchemeValue, Bool> Predicate => _predicate;
    
    public override Func<SchemeValue, object> ConvertArg => _convertArg;

    public override Func<object, SchemeValue> WrapReturn => _wrapReturn;

}
public class GenericTypeDescriptor : TypeDescriptor {
    
    public TypeDescriptor[] ChildDescriptors {get;}
    
    public Type[] TypeArguments {get;}
    
}