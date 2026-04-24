
using Jig.Types;
namespace Jig;
public class Integer(int i) : Number<int>(i), IHasTypeDescriptor {

    public static TypeDescriptor TypeDescriptor {get;} = new SchemeValueTypeDescriptor<Integer>();
    public static Integer Zero {get;} = new Integer(0);
    public static Integer One {get;} = new Integer(1);
    public static Integer Two {get;} = new Integer(2);
    
    
    public override string Print() => Value.ToString();
    public override string ToString() => Value.ToString();

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
    
    
    public static implicit operator Float(Integer i) => new Float(i.Value);


    public static Bool operator ==(Integer i1, Number n) {
        return n switch
        {
            Integer i2 => i1.Value == i2.Value ? Bool.True : Bool.False,
            Float d2 => Math.Abs(i1.Value - d2.Value) < double.Epsilon ? Bool.True : Bool.False,
            _ => throw new NotImplementedException(),
        };
    }

    public static Bool operator !=(Integer i1, Number n) {
        return n switch
        {
            Integer i2 => i1.Value != i2.Value ? Bool.True : Bool.False,
            Float d2 => Math.Abs(i1.Value - d2.Value) > double.Epsilon ? Bool.True : Bool.False,
            _ => throw new NotImplementedException(),
        };
    }
    
    public static Integer operator %(Integer z1, Integer z2) => new (z1.Value % z2.Value); // This corresponds to mod0

    public static Integer Mod(Integer z1, Integer z2) {
        int r = z1.Value % z2.Value;
        if (r == 0) return Integer.Zero;
        return (r > 0) != (z2.Value > 0) ? new Integer(r + z2.Value) : new Integer(r);
    }

    public static Integer BitwiseNot(Integer z) {
        return new Integer(~z.Value);
    }

    public static Integer BitwiseOr(Integer z1, Integer z2) {
        return new Integer(z1.Value | z2.Value);
    }

    public static Integer BitwiseXor(Integer z1, Integer z2) {
        return new Integer(z1.Value ^ z2.Value);
    }

    public static Integer BitwiseAnd(Integer z1, Integer z2) {
        return new Integer(z1.Value & z2.Value);
    }

    public static Integer BitwiseShiftLeft(Integer z1, Integer z2) {
        return new Integer(z1.Value << z2.Value);
    }

    public static Integer BitwiseShiftRight(Integer z1, Integer z2) {
        return new Integer(z1.Value >> z2.Value);
    }
    
    public static Integer operator +(Integer i1, Integer i2) => new (i1.Value + i2.Value);
    public static Float operator +(Integer i1, Float f) => new (i1.Value + f.Value);

    public static Number operator +(Integer i1, Number n) {
        // Console.WriteLine($"int + n ({i1.Print()} + {n.Print()}). n is {n.GetType()}");
        switch (n) {
            case Integer i2:
                return new Integer(i1.Value + i2.Value);
            case Float d2:
                return new Float(i1.Value + d2.Value);
            default:
                throw new NotImplementedException($"n was a {(n is null ? "null" : n.GetType().Name)}");
        }
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
            // TODO: return int if no decimal part
            Integer i2 =>
                // ReSharper disable once PossibleLossOfFraction
                Math.Abs(i1.Value / i2.Value - (double) i1.Value / i2.Value) < double.Epsilon
                ? new Integer(i1.Value / i2.Value)
                : new Float((double) i1.Value / i2.Value),
            Float d2 => new Float(i1.Value / d2.Value),
            _ => throw new NotImplementedException(),
        };
    }

    public static Number operator %(Integer i1, Number n) {
        return n switch
        {
            Integer i2 => new Integer(i1.Value % i2.Value),
            Float d2 => new Float(i1.Value % d2.Value),
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

    public static Bool operator >=(Integer i1, Number n) {
        return n switch
        {
            Integer i2 => i1.Value >= i2.Value ? Bool.True : Bool.False,
            Float d2 => i1.Value >= d2.Value ? Bool.True : Bool.False,
            _ => throw new NotImplementedException(),
        };
    }

    public static Bool operator <=(Integer i1, Number n) {
        return n switch
        {
            Integer i2 => i1.Value <= i2.Value ? Bool.True : Bool.False,
            Float d2 => i1.Value <= d2.Value ? Bool.True : Bool.False,
            _ => throw new NotImplementedException(),
        }
            ;
    }
}