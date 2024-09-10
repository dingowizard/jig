namespace Jig;

public class Procedure(Delegate d) : LiteralExpr<Delegate>(d) {
    public Thunk? Apply(Delegate k, List args)
    {
        return Value switch
        {
            Builtin builtin => builtin(k, args),
            ListFunction listFn => listFn(k, args),
            PairFunction pairFn => pairFn(k, args.ElementAt(0), List.NewList(args.Skip(1).ToArray())),
            ImproperListFunction2 improper2 => improper2(k, args.ElementAt(0), args.ElementAt(1), List.NewList(args.Skip(2).ToArray())),
            ImproperListFunction3 improper3 => improper3(k, args.ElementAt(0), args.ElementAt(1), args.ElementAt(2), List.NewList(args.Skip(3).ToArray())),
            ImproperListFunction4 improper4 => improper4(k, args.ElementAt(0), args.ElementAt(1), args.ElementAt(2), args.ElementAt(3), List.NewList(args.Skip(4).ToArray())),
            ImproperListFunction5 improper5 => improper5(k, args.ElementAt(0), args.ElementAt(1), args.ElementAt(2), args.ElementAt(3), args.ElementAt(4), List.NewList(args.Skip(5).ToArray())),
            ImproperListFunction6 improper6 => improper6(k, args.ElementAt(0), args.ElementAt(1), args.ElementAt(2), args.ElementAt(3), args.ElementAt(4), args.ElementAt(5), List.NewList(args.Skip(6).ToArray())),
            ImproperListFunction7 improper7 => improper7(k, args.ElementAt(0), args.ElementAt(1), args.ElementAt(2), args.ElementAt(3), args.ElementAt(4), args.ElementAt(5), args.ElementAt(6), List.NewList(args.Skip(7).ToArray())),
            _ => Value.DynamicInvoke(new System.Collections.Generic.List<object> {k}.Concat(args).ToArray()) as Thunk,
        };
    }

    public override string Print() => "#<procedure>";

}
