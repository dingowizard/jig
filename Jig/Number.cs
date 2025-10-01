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
}

