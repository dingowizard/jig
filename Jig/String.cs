namespace Jig;

public class String(string s) : LiteralExpr<string>(s) {
    public override string Print() {
        // TODO: handle special chars like \n
        return "\"" + Value + "\"";
    }
    public static Thunk? string_p(Delegate k, List args) {
        
        if (args is List.NonEmpty properList) {
            if (args.Count() != 1) return Builtins.Error(k, "string?: expected one argument but got {args.Count()}");
            IForm arg = properList.Car;
            if (arg is String) {
                return Continuation.ApplyDelegate(k, Bool.True);
            }
            return Continuation.ApplyDelegate(k, Bool.False);

        } else {
            return Builtins.Error(k, "string?: expected one argument but got none");
        }
    }
}

