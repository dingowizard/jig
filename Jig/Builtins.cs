namespace Jig;

public delegate void Builtin(Continuation k, List args);

internal static class Builtins {

//     public static void map (Continuation k, params object[] args) {

//         if (args.Length < 2) throw new Exception("map: expected at least two arguments -- a procedure and a list");
//         Delegate? proc = args[0] as Delegate;
//         if (proc is null) throw new Exception("map: expected first argument to be a procedure");
//         IEnumerable<object>? list = args[1] as IEnumerable<object>;
//         if (list is null) throw new Exception("map: expected second argument to be a list");
//         map_internal(k, proc, list);

//     }
//

    internal static void map_internal(Continuation k, Action<Continuation,LiteralExpr<CompiledCode>> proc, List list) {
// // (define (map/cps k fn xs)
// //   (if (null? xs)
// //       (k xs)
// //       (fn (lambda (v0) (map/cps (lambda (v1) (k (cons v0 v1))) fn (cdr xs))) (car xs))))
        if (list is List.NonEmptyList properList) {
            Continuation k2 = (v0) => map_internal(((v1) => k((Expr)Expr.Pair.Cons(v0, (List) v1))), proc, (List)properList.Cdr);
            proc.DynamicInvoke(k2, (LiteralExpr<CompiledCode>)properList.Car);
            return;
        } else {
            k(list);
            return; // TODO: why is this needed?
        }
    }

    public static void car (Continuation k, List args) {
        if (args is List.NonEmptyList properList) {
            switch (properList.Car) {
                case IPair pair:
                    k(pair.Car);
                    return;
                default:
                    throw new Exception($"car: expected pair but got {properList.Car}");
            }
        } else {
            throw new Exception("car: expected one argument but got none");
        }
    }

    public static void nullP (Continuation k, List args) {
        if (args is List.NonEmptyList properList) {
            Expr arg = properList.Car;
            if (arg is Expr.NullType) {
                k(new Expr.Boolean(true));
                return;
            }
            k(new Expr.Boolean(false));
            return;

        } else {
            throw new Exception("null?: expected one argument but got none");
        }
    }

    public static void cdr (Continuation k, List args) {
        if (args is List.NonEmptyList properList) {
            switch (properList.Car) {
                case IPair pair:
                    k(pair.Cdr);
                    return;
                default:
                    throw new Exception($"cdr: expected pair but got {properList.Cdr}");
            }
        } else {
            throw new Exception("cdr: expected one argument but got none");
        }
    }

    public static void succ(Continuation k, List args) {
        if (args is List.NonEmptyList properList) {
            object arg = properList.Car;
            if (arg is Expr.Integer intExpr) {
                k(new Expr.Integer(intExpr.Value + 1));
                return;
            } else {
                throw new Exception($"incr: expected single integer argument, but got {args}");
            }
        } else {
            throw new Exception("succ: expected one argument but got none");
        }
    }

    public static void cons (Continuation k, List args) {
        if (args is List.NonEmptyList properList) {
            if (properList.Count() != 2) {
                throw new Exception("cons: expected two arguments");
            }
            Expr car = args.ElementAt(0);
            Expr cdr = args.ElementAt(1);
            k((Expr)Expr.Pair.Cons(car, cdr));
            return;
        } else {
            throw new Exception("car: expected one argument but got none");
        }


    }

    public static void apply (Continuation k, LiteralExpr<Delegate> del, List args) {
        switch (del.Value) {
            case Builtin builtin:
                builtin(k, args);
                return;
            case ListFunction listFn:
                listFn(k, args);
                return;
            case PairFunction pairFn:
                Console.WriteLine("pairFn");
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
                del.Value.DynamicInvoke(new List<object>{k}.Concat(args).ToArray());
                return;
        }
    }
}
