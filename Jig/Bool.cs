namespace Jig;

public class Bool : LiteralExpr<bool> {

    private Bool(bool b) : base(b) {}
    public static readonly Bool True = new (true);
    public static readonly Bool False = new (false);
    public override string Print() => Value ? "#t" : "#f";

    public static Thunk? boolean_p(Delegate k, List args) {
        
        if (args is List.NonEmpty properList) {
            if (args.Count() != 1) return Builtins.Error(k, "boolean?: expected one argument but got {args.Count()}");
            IForm arg = properList.Car;
            if (arg is Bool) {
                return Continuation.ApplyDelegate(k, Bool.True);
            }
            return Continuation.ApplyDelegate(k, Bool.False);

        } else {
            return Builtins.Error(k, "boolean?: expected one argument but got none");
        }
    }

    public static Thunk? boolean_eq_p(Delegate k, IForm arg1, IForm arg2, List args) {
        if (arg1 is Bool b1 && arg2 is Bool b2) {
            if (!b1.Equals(b2)) {
                return Continuation.ApplyDelegate(k, Bool.False);
            } 
            while (args is List.NonEmpty properList) {
                if (properList.Car is Bool b) {
                    if (!b2.Equals(b)) {
                        return Continuation.ApplyDelegate(k, Bool.False);
                    }
                }
                args = properList.Rest;
            } 
            return Continuation.ApplyDelegate(k, Bool.True);
            
        }
        else {
            return Builtins.Error(k, $"boolean=?: expected only boolean arguments");
        }
        
    }
}