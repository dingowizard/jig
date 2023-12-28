namespace Jig;

public class Procedure : LiteralExpr<Delegate> {

    public Procedure(Delegate d) : base (d) {}

    public void Apply(Delegate k, List args) {
        switch (Value) {
            case Builtin builtin:
                builtin(k, args);
                return;
            case ListFunction listFn:
                listFn(k, args);
                return;
            case PairFunction pairFn:
                pairFn(k, args.ElementAt(0), List.NewList(args.Skip(1).ToArray()));
                return;
            case ImproperListFunction2 improper2:
                improper2(k, args.ElementAt(0), args.ElementAt(1), List.NewList(args.Skip(2).ToArray()));
                return;
            case ImproperListFunction3 improper3:
                improper3(k, args.ElementAt(0), args.ElementAt(1), args.ElementAt(2), List.NewList(args.Skip(3).ToArray()));
                return;
            case ImproperListFunction4 improper4:
                improper4(k, args.ElementAt(0), args.ElementAt(1), args.ElementAt(2), args.ElementAt(3), List.NewList(args.Skip(4).ToArray()));
                return;
            case ImproperListFunction5 improper5:
                improper5(k, args.ElementAt(0), args.ElementAt(1), args.ElementAt(2), args.ElementAt(3), args.ElementAt(4), List.NewList(args.Skip(5).ToArray()));
                return;
            case ImproperListFunction6 improper6:
                improper6(k, args.ElementAt(0), args.ElementAt(1), args.ElementAt(2), args.ElementAt(3), args.ElementAt(4), args.ElementAt(5), List.NewList(args.Skip(6).ToArray()));
                return;
            case ImproperListFunction7 improper7:
                improper7(k, args.ElementAt(0), args.ElementAt(1), args.ElementAt(2), args.ElementAt(3), args.ElementAt(4), args.ElementAt(5), args.ElementAt(6), List.NewList(args.Skip(7).ToArray()));
                return;
            default:
                Value.DynamicInvoke(new List<object>{k}.Concat(args).ToArray());
                return;
        }

    }

    public override string Print() => "#<procedure>";

}
