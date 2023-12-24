using System.Linq.Expressions;
using System.Reflection;

namespace Jig;

public delegate void Builtin(Delegate k, List args);

internal static class Builtins {

    internal static void map_internal(Continuation k, Action<Continuation, LiteralExpr<CompiledCode>> proc, List list) {
// // (define (map/cps k fn xs)
// //   (if (null? xs)
// //       (k xs)
// //       (fn (lambda (v0) (map/cps (lambda (v1) (k (cons v0 v1))) fn (cdr xs))) (car xs))))
        if (list is List.NonEmptyList properList) {
            Continuation k2 = (v0) => map_internal((Continuation)((v1) => k((Expr)Expr.Pair.Cons(v0, (List) v1))), proc, (List)properList.Cdr);
            proc(k2, (LiteralExpr<CompiledCode>)properList.Car);
            return;
        } else {
            k(list); // at this point , k is (lambda (l) (apply (car l) (cdr l)))
            return; // TODO: why is this needed?
        }
    }

    public static void car (Delegate k, List args) {
        if (args is List.NonEmptyList properList) {
            switch (properList.Car) {
                case IPair pair:
                    ApplyContinuation(k, pair.Car);
                    return;
                default:
                    throw new Exception($"car: expected pair but got {properList.Car}");
            }
        } else {
            throw new Exception("car: expected one argument but got none");
        }
    }

    public static void nullP (Delegate k, List args) {
        if (args is List.NonEmptyList properList) {
            Expr arg = properList.Car;
            if (arg is Expr.NullType) {
                ApplyContinuation(k, new Expr.Boolean(true));
                return;
            }
            ApplyContinuation(k, new Expr.Boolean(false));
            return;

        } else {
            throw new Exception("null?: expected one argument but got none");
        }
    }

    public static void cdr (Delegate k, List args) {
        if (args is List.NonEmptyList properList) {
            switch (properList.Car) {
                case IPair pair:
                    ApplyContinuation(k, pair.Cdr);
                    return;
                default:
                    throw new Exception($"cdr: expected pair but got {properList.Cdr}");
            }
        } else {
            throw new Exception("cdr: expected one argument but got none");
        }
    }

    public static void succ(Delegate k, List args) {
        if (args is List.NonEmptyList properList) {
            object arg = properList.Car;
            if (arg is Expr.Integer intExpr) {
                ApplyContinuation(k, new Expr.Integer(intExpr.Value + 1));
                return;
            } else {
                throw new Exception($"incr: expected single integer argument, but got {args}");
            }
        } else {
            throw new Exception("succ: expected one argument but got none");
        }
    }

    public static void sum(Delegate k, List args) {
        Expr.Integer intResult = new Expr.Integer(0);
        bool resultIsInt = true;
        Expr.Double doubleResult = new Expr.Double(0.0);
        foreach (var arg in args) {
            if (resultIsInt) {
                if (arg is Expr.Integer i) {
                    intResult += i;
                    continue;
                } else if (arg is Expr.Double d) {
                    resultIsInt = false;
                    doubleResult = intResult + d;
                    continue;
                } else {
                    throw new Exception($"+: all args must be numbers. {arg} is not a number.");
                }
            } else {
                if (arg is Expr.Integer i) {
                    doubleResult += i;
                    continue;
                } else if (arg is Expr.Double d) {
                    doubleResult += d;
                    continue;
                } else {
                    throw new Exception($"+: all args must be numbers. {arg} is not a number.");
                }

            }
        }
        ApplyContinuation(k, resultIsInt ? intResult : doubleResult);
        return;

    }

    public static void diff(Delegate k, Expr first, List args) {
        bool resultIsInt = true;
        Expr.Integer intResult = new Expr.Integer(0);
        Expr.Double doubleResult = new Expr.Double(0.0);
        if (first is Expr.Integer i) {
            intResult = i;
        } else if (first is Expr.Double d) {
            doubleResult = d;
            resultIsInt = false;
        } else {
            throw new Exception($"-: all args must be numbers. {first} is not a number.");
        }
        foreach (var arg in args) {
            if (resultIsInt) {
                if (arg is Expr.Integer x) {
                    intResult -= x;
                    continue;
                } else if (arg is Expr.Double d) {
                    resultIsInt = false;
                    doubleResult = intResult - d;
                    continue;
                } else {
                    throw new Exception($"-: all args must be numbers. {arg} is not a number.");
                }
            } else {
                if (arg is Expr.Integer x) {
                    doubleResult -= x;
                    continue;
                } else if (arg is Expr.Double d) {
                    doubleResult -= d;
                    continue;
                } else {
                    throw new Exception($"-: all args must be numbers. {arg} is not a number.");
                }
            }
        }
        ApplyContinuation(k, resultIsInt ? intResult : doubleResult);
        return;
    }

    public static void numEq(Delegate k, Expr first, List args) {
        // TODO: should 1 = 1.0? it does in chez. if so best way to implement
        if (first is Expr.Integer i) {
            Expr.Integer current = i;
            foreach(var arg in args) {
                if (arg is Expr.Integer nextInt) {
                    if (current.Equals(nextInt)) {
                        continue;
                    } else {
                        ApplyContinuation(k, new Expr.Boolean(false));
                        return;
                    }
                } else if (arg is Expr.Double) {
                    ApplyContinuation(k, new Expr.Boolean(false));
                    return;
                } else {
                    throw new Exception($"=: expected numeric arguments, but got {arg}");
                }
            }
            ApplyContinuation(k, new Expr.Boolean(true));
            return;
        } else if (first is Expr.Double d) {
            Expr.Double current = d;
            foreach(var arg in args) {
                if (arg is Expr.Double nextD) {
                    if (current.Equals(nextD)) {
                        continue;
                    } else {
                        ApplyContinuation(k, new Expr.Boolean(false));
                        return;
                    }
                } else if (arg is Expr.Integer) {
                    ApplyContinuation(k, new Expr.Boolean(false));
                    return;
                } else {
                    throw new Exception($"=: expected numeric arguments, but got {arg}");
                }
            }
            ApplyContinuation(k, new Expr.Boolean(true));
            return;

        } else {
            throw new Exception($"=: expects only numeric arguments. Got {first}");
        }
    }

    public static void product(Delegate k, List args) {
        Expr.Integer intResult = new Expr.Integer(1);
        bool resultIsInt = true;
        Expr.Double doubleResult = new Expr.Double(1.0);
        foreach (var arg in args) {
            if (resultIsInt) {
                if (arg is Expr.Integer i) {
                    intResult *= i;
                    continue;
                } else if (arg is Expr.Double d) {
                    resultIsInt = false;
                    doubleResult = intResult * d;
                    continue;
                } else {
                    throw new Exception($"*: all args must be numbers. {arg} is not a number.");
                }
            } else {
                if (arg is Expr.Integer i) {
                    doubleResult *= i;
                    continue;
                } else if (arg is Expr.Double d) {
                    doubleResult *= d;
                    continue;
                } else {
                    throw new Exception($"*: all args must be numbers. {arg} is not a number.");
                }

            }
        }
        ApplyContinuation(k, resultIsInt ? intResult : doubleResult);
        return;
    }

    public static void cons (Delegate k, List args) {
        if (args is List.NonEmptyList properList) {
            if (properList.Count() != 2) {
                throw new Exception("cons: expected two arguments");
            }
            Expr car = args.ElementAt(0);
            Expr cdr = args.ElementAt(1);
            Expr result = (Expr)Expr.Pair.Cons(car, cdr);
            ApplyContinuation(k, result);
            return;
        } else {
            throw new Exception("cons: expected two arguments but got none");
        }
    }

    internal static void ApplyContinuation(Delegate k, Expr arg) {
            if (k is ContinuationAny cany) {
                cany(arg);
                return;
            }
            k.DynamicInvoke(arg);
            return;
    }

    public static void values(Delegate k, List args) {
        switch (k) {
            case ContinuationAny cany:
                cany(args.ToArray());
                return;
            case ListContinuation lc: // TODO: should there be both continuationany and listcontinuation types?
                lc(args);
                return;
            case PairContinuation pc:
                pc(args.ElementAt(0),  List.NewList(args.Skip(1).ToArray()));
                return;
            case ImproperListContinuation2 ic2:
                ic2(args.ElementAt(0), args.ElementAt(1), List.NewList(args.Skip(2).ToArray()));
                return;
            case ImproperListContinuation3 ic3:
                ic3(args.ElementAt(0), args.ElementAt(1), args.ElementAt(2), List.NewList(args.Skip(3).ToArray()));
                return;
            case ImproperListContinuation4 ic4:
                ic4(args.ElementAt(0), args.ElementAt(1), args.ElementAt(2), args.ElementAt(3), List.NewList(args.Skip(4).ToArray()));
                return;
            case ImproperListContinuation5 ic5:
                ic5(args.ElementAt(0), args.ElementAt(1), args.ElementAt(2), args.ElementAt(3), args.ElementAt(4), List.NewList(args.Skip(5).ToArray()));
                return;
            case ImproperListContinuation6 ic6:
                ic6(args.ElementAt(0), args.ElementAt(1), args.ElementAt(2), args.ElementAt(3), args.ElementAt(4), args.ElementAt(5), List.NewList(args.Skip(6).ToArray()));
                return;
            case ImproperListContinuation7 ic7:
                ic7(args.ElementAt(0), args.ElementAt(1), args.ElementAt(2), args.ElementAt(3), args.ElementAt(4), args.ElementAt(5), args.ElementAt(6), List.NewList(args.Skip(7).ToArray()));
                return;
            default:
                k.DynamicInvoke(args.ToArray());
                return;
        }

    }

    public static void callcc(Delegate k, List args) {
        if (args.Count() != 1) throw new Exception($"call/cc: expected one argument.");
        LiteralExpr<Delegate> proc = args.ElementAt(0) as LiteralExpr<Delegate> ?? throw new Exception("call/cc: expected procedure argument but got {args.ElementAt(0)}");
        Action<Delegate, Expr> del = proc.Value as Action<Delegate, Expr> ?? throw new Exception("call/cc: expected procedure with one parameter but got {proc}");
        apply(k, proc, List.NewList(new LiteralExpr<Delegate>(k)));
        return;
    }

    public static void call_with_values(Delegate k, LiteralExpr<Delegate> producerExpr, LiteralExpr<Delegate> consumerExpr) {
        var producer = producerExpr.Value; // the producer is a thunk, but internally that is something like (lambda (k) ...)
        var consumer = consumerExpr.Value;
        Delegate cont = ContinuationFromProc(k, consumer);
        Action<Delegate> action = producer as Action<Delegate> ?? throw new Exception("call-with-values: expected first argument to be a thunk.");
        action(cont);
        return;
        // apply(cont, producerExpr, List.Empty);
    }

    private delegate void ListContinuation(List rest);
    private delegate void PairContinuation(Expr arg0, List rest);
    private delegate void ImproperListContinuation2(Expr arg0, Expr arg1, List rest);
    private delegate void ImproperListContinuation3(Expr arg0, Expr arg1, Expr arg2, List rest);
    private delegate void ImproperListContinuation4(Expr arg0, Expr arg1, Expr arg2, Expr arg3, List rest);
    private delegate void ImproperListContinuation5(Expr arg0, Expr arg1, Expr arg2, Expr arg3, Expr arg4, List rest);
    private delegate void ImproperListContinuation6(Expr arg0, Expr arg1, Expr arg2, Expr arg3, Expr arg4, Expr arg5, List rest);
    private delegate void ImproperListContinuation7(Expr arg0, Expr arg1, Expr arg2, Expr arg3, Expr arg4, Expr arg5, Expr arg6, List rest);

    public static Delegate ContinuationFromProc(Delegate k, Delegate proc) {
        MethodInfo method = proc.GetType().GetMethod("Invoke") ?? throw new Exception($"ContinuationFromProc: could not find 'Invoke' method on type of proc (proc.GetType())");
        var parameterInfos = method.GetParameters().Skip(1); // get parameters that are not the continuation
        var paramList = new List<ParameterExpression>();
        foreach (var p in parameterInfos)
        {
            paramList.Add(Expression.Parameter(typeof(Expr), p.ToString()));
        }
        // TODO: handle situation when lambda expression has a rest param Eg (lambda (a . rest) rest) or (lambda l l)
        Type? type = GetTypeForContinuation(proc);
        if (type is null) {
            LambdaExpression lexpr = Expression.Lambda(
                body: ET.DynInv(new Expression [] {Expression.Constant(proc), Expression.Constant(k)}.Concat(paramList).ToArray()),
                parameters: paramList.ToArray()
            );
            return lexpr.Compile();
        }
        return Expression.Lambda(
            delegateType: type,
            body: ET.DynInv(new Expression [] {Expression.Constant(proc), Expression.Constant(k)}.Concat(paramList).ToArray()),
            parameters: paramList.ToArray()
        ).Compile();
        throw new NotImplementedException();
    }

    private static Type? GetTypeForContinuation(Delegate proc)
    {
        switch (proc) {
            case ListFunction _:
                return typeof(ListContinuation);
            case PairFunction _:
                return typeof(PairContinuation);
            case ImproperListFunction2 _:
                return typeof(ImproperListContinuation2);
            case ImproperListFunction3 _:
                return typeof(ImproperListContinuation3);
            case ImproperListFunction4 _:
                return typeof(ImproperListContinuation4);
            case ImproperListFunction5 _:
                return typeof(ImproperListContinuation5);
            case ImproperListFunction6 _:
                return typeof(ImproperListContinuation6);
            case ImproperListFunction7 _:
                return typeof(ImproperListContinuation7);
            default:
                return null;
        }
    }

    public static void apply (Delegate k, Expr proc, List args) {
        LiteralExpr<Delegate> del = proc as LiteralExpr<Delegate> ?? throw new Exception($"apply: expected procedure as first argument, but got {proc}");
        switch (del.Value) {
            case Continuation cont: // TODO: couldn't other types of continuation be applied?
                cont(args.ElementAt(0));
                return;
            case ContinuationAny cany:
                cany(args.ToArray());
                return;
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
                del.Value.DynamicInvoke(new List<object>{k}.Concat(args).ToArray());
                return;
        }
    }
}
