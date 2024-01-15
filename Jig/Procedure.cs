namespace Jig;

public class Procedure : LiteralExpr<Delegate> {
    // TODO: are we sure that the procedures themselves should NOT be actions?
    // lambda exprs have to return MaybeThunks in order to be trampolined
    // what about builtins? couldn't they be Actions since they don't have to return thunks?
    // but then what would the signature of Apply be?

    public Procedure(Delegate d) : base (d) {}

    public Thunk Apply(Delegate k, List args) {
        switch (Value) {
            case Builtin builtin:
                return builtin(k, args);
            case ListFunction listFn:
                return listFn(k, args);
            case PairFunction pairFn:
                return pairFn(k, args.ElementAt(0), List.NewList(args.Skip(1).ToArray()));
            case ImproperListFunction2 improper2:
                return improper2(k, args.ElementAt(0), args.ElementAt(1), List.NewList(args.Skip(2).ToArray()));
            case ImproperListFunction3 improper3:
                return improper3(k, args.ElementAt(0), args.ElementAt(1), args.ElementAt(2), List.NewList(args.Skip(3).ToArray()));
            case ImproperListFunction4 improper4:
                return improper4(k, args.ElementAt(0), args.ElementAt(1), args.ElementAt(2), args.ElementAt(3), List.NewList(args.Skip(4).ToArray()));
            case ImproperListFunction5 improper5:
                return improper5(k, args.ElementAt(0), args.ElementAt(1), args.ElementAt(2), args.ElementAt(3), args.ElementAt(4), List.NewList(args.Skip(5).ToArray()));
            case ImproperListFunction6 improper6:
                return improper6(k, args.ElementAt(0), args.ElementAt(1), args.ElementAt(2), args.ElementAt(3), args.ElementAt(4), args.ElementAt(5), List.NewList(args.Skip(6).ToArray()));
            case ImproperListFunction7 improper7:
                return improper7(k, args.ElementAt(0), args.ElementAt(1), args.ElementAt(2), args.ElementAt(3), args.ElementAt(4), args.ElementAt(5), args.ElementAt(6), List.NewList(args.Skip(7).ToArray()));
            default:
                return (Thunk)Value.DynamicInvoke(new List<object>{k}.Concat(args).ToArray());
        }

    }

    public override string Print() => "#<procedure>";

}
