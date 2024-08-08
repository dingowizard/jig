namespace Jig;
public class Float(double d) : Number<double>(d) {
    public override bool Equals(object? obj) {
        if (obj is null) return false;
        if (obj is Float lit) {
            return this.Value.Equals(lit.Value);
        }
        return false;
    }

    public override int GetHashCode() {
        return Value.GetHashCode();
    }

    public static Bool operator ==(Float d1, Number n) {
        return n switch
        {
            Integer i2 => d1.Value == i2.Value ? Bool.True : Bool.False,
            Float d2 => d1.Value == d2.Value ? Bool.True : Bool.False,
            _ => throw new NotImplementedException(),
        };
    }

    public static Bool operator !=(Float d1, Number n) {
        return n switch
        {
            Integer i2 => d1.Value != i2.Value ? Bool.True : Bool.False,
            Float d2 => d1.Value != d2.Value ? Bool.True : Bool.False,
            _ => throw new NotImplementedException(),
        };
    }
    public static Number operator +(Float d1, Number n) {
        // TODO: is it  better to use overrides rather than switch statements?
        return n switch
        {
            Integer i2 => new Float(d1.Value + i2.Value),
            Float d2 => new Float(d1.Value + d2.Value),
            _ => throw new NotImplementedException(),
        };
    }

    public static Number operator -(Float d1, Number n) {
        return n switch
        {
            Integer i2 => new Float(d1.Value - i2.Value),
            Float d2 => new Float(d1.Value - d2.Value),
            _ => throw new NotImplementedException(),
        };
    }

    public static Number operator *(Float d1, Number n) {
        return n switch
        {
            Integer i2 => new Float(d1.Value * i2.Value),
            Float d2 => new Float(d1.Value * d2.Value),
            _ => throw new NotImplementedException(),
        };
    }

    public static Number operator /(Float d1, Number n) {
        return n switch
        {
            Integer i2 => new Float(d1.Value / i2.Value),
            Float d2 => new Float(d1.Value / d2.Value),
            _ => throw new NotImplementedException(),
        };
    }

    public static Bool operator >(Float d1, Number n) {
        return n switch
        {
            Integer i2 => d1.Value > i2.Value ? Bool.True : Bool.False,
            Float d2 => d1.Value > d2.Value ? Bool.True : Bool.False,
            _ => throw new NotImplementedException(),
        };
    }

    public static Bool operator <(Float d1, Number n) {
        return n switch
        {
            Integer i2 => d1.Value < i2.Value ? Bool.True : Bool.False,
            Float d2 => d1.Value < d2.Value ? Bool.True : Bool.False,
            _ => throw new NotImplementedException(),
        };
    }

}