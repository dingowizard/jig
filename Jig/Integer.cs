namespace Jig;
public class Integer(int i) : Number<int>(i) {

    public static Integer Zero {get;} = new Integer(0);
    public static Integer One {get;} = new Integer(1);
    public static Integer Two {get;} = new Integer(2);

    public override bool Equals(object? obj) {
        if (obj is null) return false;
        if (obj is Integer lit) {
            return this.Value.Equals(lit.Value);
        }
        return false;
    }

    public override int GetHashCode() {
        return Value.GetHashCode();
    }


    public static Bool operator ==(Integer i1, Number n) {
        return n switch
        {
            Integer i2 => i1.Value == i2.Value ? Bool.True : Bool.False,
            Float d2 => i1.Value == d2.Value ? Bool.True : Bool.False,
            _ => throw new NotImplementedException(),
        };
    }

    public static Bool operator !=(Integer i1, Number n) {
        return n switch
        {
            Integer i2 => i1.Value != i2.Value ? Bool.True : Bool.False,
            Float d2 => i1.Value != d2.Value ? Bool.True : Bool.False,
            _ => throw new NotImplementedException(),
        };
    }

    public static Integer operator +(Integer i1, Integer i2) => new (i1.Value + i2.Value);
    public static Float operator +(Integer i1, Float f) => new (i1.Value + f.Value);

    public static Number operator +(Integer i1, Number n) {
        return n switch
        {
            Integer i2 => new Integer(i1.Value + i2.Value),
            Float d2 => new Float(i1.Value + d2.Value),
            _ => throw new NotImplementedException(),
        };
    }

    public static Number operator -(Integer i1, Number n) {
        return n switch
        {
            Integer i2 => new Integer(i1.Value - i2.Value),
            Float d2 => new Float(i1.Value - d2.Value),
            _ => throw new NotImplementedException(),
        };
    }

    public static Number operator *(Integer i1, Number n) {
        return n switch
        {
            Integer i2 => new Integer(i1.Value * i2.Value),
            Float d2 => new Float(i1.Value * d2.Value),
            _ => throw new NotImplementedException(),
        };
    }

    public static Number operator /(Integer i1, Number n) {
        return n switch
        {
            Integer i2 => new Integer(i1.Value / i2.Value),
            Float d2 => new Float(i1.Value / d2.Value),
            _ => throw new NotImplementedException(),
        };
    }

    public static Bool operator >(Integer i1, Number n) {
        return n switch
        {
            Integer i2 => i1.Value > i2.Value ? Bool.True : Bool.False,
            Float d2 => i1.Value > d2.Value ? Bool.True : Bool.False,
            _ => throw new NotImplementedException(),
        };
    }

    public static Bool operator <(Integer i1, Number n) {
        return n switch
        {
            Integer i2 => i1.Value < i2.Value ? Bool.True : Bool.False,
            Float d2 => i1.Value < d2.Value ? Bool.True : Bool.False,
            _ => throw new NotImplementedException(),
        };
    }

}