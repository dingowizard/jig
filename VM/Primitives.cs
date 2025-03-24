using Jig;

namespace VM;

public static class Primitives {
    private static Form car(Form form) {
        if (form is IPair p) {
            return (Form)p.Car;
        }
        throw new Exception("car: expected argument to be pair, got {form}");
    }
    public static Primitive Car { get; } = new Primitive(car);

    private static Form cdr(Form form) {
        if (form is IPair p) {
            return (Form)p.Cdr;
        }
        throw new Exception("cdr: expected argument to be pair, got {form}");
    }

    public static Primitive Cdr { get; } = new Primitive(cdr);
    
    private static Form cons(Form car, Form cdr) {
        return (Form)Pair.Cons(car, cdr);
    }

    public static Primitive Cons { get; } = new Primitive(cons);
    
    private static Form zerop(Form form) {
        if (form is Number number) {
            return number == Integer.Zero;
        }
        throw new Exception("zero?: expected argument to be number, got {form}");
    }

    public static Primitive ZeroP { get; } = new Primitive(zerop);
    
    private static Form nullp(Form form) {
        if (form is List xs) {
            return xs.NullP;
        }
        return Bool.False;

    }

    public static Primitive NullP { get; } = new Primitive(nullp);
}

public class Primitive : Form {
    public Primitive(Delegate fn) {
        Delegate = fn;
    }

    public Delegate Delegate { get;}
    public override string Print() => "#<procedure>";

}