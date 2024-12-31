namespace Jig;

public abstract class Number : LiteralExpr {
    private static bool IsZero(Number n) {
        return n switch {
            Integer i => i.Value == 0,
            Float f => f.Value == 0,
            _ => throw new NotImplementedException()
        };
    }
    
    

    public static Number From(int i) {
        return new Integer(i);
    }

    public static Number From(double d) {
        return new Float(d);
    }

    public abstract override int GetHashCode();

    public abstract override bool Equals(object? obj);

    public static Bool operator ==(Number n1, Number n2) {
        return n1 switch
        {
            Integer in1 => in1 == n2,
            Float d1 => d1 == n2,
            _ => throw new NotImplementedException(),
        };
    }

    public static Bool operator !=(Number n1, Number n2) {
        return n1 switch
        {
            Integer in1 => in1 != n2,
            Float d1 => d1 != n2,
            _ => throw new NotImplementedException(),
        };
    }

    public static Number operator +(Number n1, Number n2) {
        return n1 switch
        {
            Integer in1 => in1 + n2,
            Float d1 => d1 + n2,
            _ => throw new NotImplementedException(),
        };
    }

    public static Number operator -(Number n1, Number n2) {
        return n1 switch
        {
            Integer in1 => in1 - n2,
            Float d1 => d1 - n2,
            _ => throw new NotImplementedException(),
        };
    }

    public static Number operator *(Number n1, Number n2) {
        return n1 switch
        {
            Integer in1 => in1 * n2,
            Float d1 => d1 * n2,
            _ => throw new NotImplementedException(),
        };
    }

    public static Number operator /(Number n1, Number n2) {
        return n1 switch
        {
            Integer in1 => in1 / n2,
            Float d1 => d1 / n2,
            _ => throw new NotImplementedException(),
        };
    }
    public static Number operator %(Number n1, Number n2) {
        return n1 switch
        {
            Integer in1 => in1 % n2,
            Float d1 => d1 % n2,
            _ => throw new NotImplementedException(),
        };
    }

    public static Bool operator >(Number n1, Number n2) {
        return n1 switch
        {
            Integer in1 => in1 > n2,
            Float d1 => d1 > n2,
            _ => throw new NotImplementedException(),
        };
    }

    public static Bool operator >=(Number n1, Number n2) {
        return n1 switch
        {
            Integer in1 => in1 >= n2,
            Float d1 => d1 >= n2,
            _ => throw new NotImplementedException(),
        };
    }
    public static Bool operator <(Number n1, Number n2) {
        return n1 switch
        {
            Integer in1 => in1 < n2,
            Float d1 => d1 < n2,
            _ => throw new NotImplementedException(),
        };
    }
    
    public static Bool operator <=(Number n1, Number n2) {
        return n1 switch
        {
            Integer in1 => in1 <= n2,
            Float d1 => d1 <= n2,
            _ => throw new NotImplementedException(),
        };
    }
    public static Thunk? product(Delegate k, List args) {
        Number acc = Integer.One;
        foreach (var arg in args) {
            if (arg is Number num) {
                acc = acc * num;
            } else {
                return Builtins.Error(k, $"*: all args must be numbers. {arg} is not a number.");
            }
        }
        return Continuation.ApplyDelegate(k, acc);

    }
    public static Thunk? diff(Delegate k, IForm first, List args) {
        if (!args.Any()) {
            if (first is Number n) {
                return Continuation.ApplyDelegate(k, Integer.Zero - n);
            }
            return Builtins.Error(k, $"-: all args must be numbers. {first} is not a number.");
        }
        if (first is Number acc) {
            foreach (var arg in args) {
                if (arg is Number num) {
                    acc = acc - num;
                } else {
                    return Builtins.Error(k, $"-: all args must be numbers. {first} is not a number.");
                }
            }
            return Continuation.ApplyDelegate(k, acc);
        } else {
            return Builtins.Error(k, $"-: all args must be numbers. {first} is not a number.");
        }
    }
    
    public static Thunk? divide(Delegate k, IForm first, List args) {
        // TODO: (/ 13 2) should yield 6.5 not 6
        // TODO: actually it should yield rational number
        if (!args.Any()) {
            if (first is Number n) {
                if (Number.IsZero(n)) {
                    return Builtins.Error(k, $"/: division by zero is undefined.");
                }
                return Continuation.ApplyDelegate(k, Integer.One / n);

            }
            else {
                    return Builtins.Error(k, $"/: all args must be numbers. {first} is not a number.");
            }
        }
        if (first is Number acc) {
            foreach (var arg in args) {
                if (arg is Number num) {
                    
                    if (Number.IsZero(num)) {
                        return Builtins.Error(k, $"/: division by zero is undefined.");
                    }
                    acc /= num;
                } else {
                    return Builtins.Error(k, $"/: all args must be numbers. {first} is not a number.");
                }
            }
            return Continuation.ApplyDelegate(k, acc);
        } else {
            return Builtins.Error(k, $"/: all args must be numbers. {first} is not a number.");
        }
    }
    public static Thunk? mod(Delegate k, List args) {
        // TODO: fix (mod 12.7 5). only integers work now.
        if (args is List.NonEmpty properList) {
            if (properList.Count() != 2) {
                return Builtins.Error(k, "mod: expected two arguments");
            }
            if (args.ElementAt(0) is not Number first
                || args.ElementAt(1) is not Number second) {
                return Builtins.Error(k, "mod: expected both arguments to be numbers");
            }
            return Continuation.ApplyDelegate(k, first % second);
        } else {
            return Builtins.Error(k, "mod: expected two arguments but got none");
        }
    }

    public static Thunk? gt(Delegate k, IList args)
    {
        if (args is not List.NonEmpty nonEmpty)
            return Builtins.Error(k, ">: expected at least one argument but got none");
        if (nonEmpty.Car is not Number first) return Builtins.Error(k, $">: expected arguments to be numbers but got {nonEmpty.Car}");
        args = nonEmpty.Rest;
        while (args is List.NonEmpty rest) {
            if (rest.Car is not Number second) return Builtins.Error(k, $">: expected arguments to be numbers but got {rest.Car}");
            Bool b = first > second;
            if (!b.Value) return Continuation.ApplyDelegate(k, b);
            first = second;
            args = rest.Rest;

        }
        return Continuation.ApplyDelegate(k, Bool.True);

    }
    public static Thunk? gte(Delegate k, IList args)
    {
        if (args is not List.NonEmpty nonEmpty)
            return Builtins.Error(k, ">=: expected at least one argument but got none");
        if (nonEmpty.Car is not Number first) return Builtins.Error(k, $">=: expected arguments to be numbers but got {nonEmpty.Car}");
        args = nonEmpty.Rest;
        while (args is List.NonEmpty rest) {
            if (rest.Car is not Number second) return Builtins.Error(k, $">=: expected arguments to be numbers but got {rest.Car}");
            Bool b = first >= second;
            if (!b.Value) return Continuation.ApplyDelegate(k, b);
            first = second;
            args = rest.Rest;

        }
        return Continuation.ApplyDelegate(k, Bool.True);

    }
    public static Thunk? integer_p(Delegate k, List args) {
        if (args is List.NonEmpty properList) {
            if (args.Count() != 1) return Builtins.Error(k, "integer?: expected one argument but got {args.Count()}");
            return Continuation.ApplyDelegate(k, properList.Car is Integer ? Bool.True : Bool.False);
        } else {
            return Builtins.Error(k, "integer?: expected one argument but got none");
        }
    }

    public static Thunk? real_p(Delegate k, List args) {
        if (args is List.NonEmpty properList) {
            if (args.Count() != 1) return Builtins.Error(k, "real?: expected one argument but got {args.Count()}");
            return Continuation.ApplyDelegate(k, properList.Car is Integer or Float ? Bool.True : Bool.False);
        } else {
            return Builtins.Error(k, "real?: expected one argument but got none");
        }
    }

    public static Thunk? floor(Delegate k, List args) {
        if (args is List.NonEmpty properList) {
            if (args.Count() != 1) return Builtins.Error(k, "floor: expected one argument but got {args.Count()}");
            if (properList.Car is Number num) {
                switch (num) {
                    case Integer i:
                        return Continuation.ApplyDelegate(k, i);
                    case Float f:
                        return Continuation.ApplyDelegate(k, new Float(Math.Floor(f.Value)));
                    default: throw new NotImplementedException();
                }
            }
            return Builtins.Error(k, "floor: expected argument to be a number, but got {properList.Car}");
        } else {
            return Builtins.Error(k, "floor: expected one argument but got none");
        }
    }
    public static Thunk? lt(Delegate k, List args) {
        if (args is not List.NonEmpty nonEmpty)
            return Builtins.Error(k, "<: expected at least one argument but got none");
        if (nonEmpty.Car is not Number first) return Builtins.Error(k, $"<: expected arguments to be numbers but got {nonEmpty.Car}");
        args = nonEmpty.Rest;
        while (args is List.NonEmpty rest) {
            if (rest.Car is not Number second) return Builtins.Error(k, $"<: expected arguments to be numbers but got {rest.Car}");
            Bool b = first < second;
            if (!b.Value) return Continuation.ApplyDelegate(k, b);
            first = second;
            args = rest.Rest;

        }
        return Continuation.ApplyDelegate(k, Bool.True);
    }

    public static Thunk? lte(Delegate k, List args) {
        
        if (args is not List.NonEmpty nonEmpty)
            return Builtins.Error(k, "<=: expected at least one argument but got none");
        if (nonEmpty.Car is not Number first) return Builtins.Error(k, $"<=: expected arguments to be numbers but got {nonEmpty.Car}");
        args = nonEmpty.Rest;
        while (args is List.NonEmpty rest) {
            if (rest.Car is not Number second) return Builtins.Error(k, $"<=: expected arguments to be numbers but got {rest.Car}");
            Bool b = first <= second;
            if (!b.Value) return Continuation.ApplyDelegate(k, b);
            first = second;
            args = rest.Rest;

        }
        return Continuation.ApplyDelegate(k, Bool.True);
    }

    public static Thunk? ceiling(Delegate k, List args) {
        // TODO: make floor and ceiling methods on number
        if (args is List.NonEmpty properList) {
            if (args.Count() != 1) return Builtins.Error(k, "ceiling: expected one argument but got {args.Count()}");
            if (properList.Car is Number num) {
                switch (num) {
                    case Integer i:
                        return Continuation.ApplyDelegate(k, i);
                    case Float f:
                        return Continuation.ApplyDelegate(k, new Float(Math.Ceiling(f.Value)));
                    default: throw new NotImplementedException();
                }
            }
            return Builtins.Error(k, "ceiling: expected argument to be a number, but got {properList.Car}");
        } else {
            return Builtins.Error(k, "ceiling: expected one argument but got none");
        }
    }
}

