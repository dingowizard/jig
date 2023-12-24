namespace Jig;

public delegate void Builtin(Delegate k, List args);

internal static class Builtins {

    internal static void map_internal(Continuation.OneArgDelegate k, Action<Continuation.OneArgDelegate, LiteralExpr<CompiledCode>> proc, List list) {
// // (define (map/cps k fn xs)
// //   (if (null? xs)
// //       (k xs)
// //       (fn (lambda (v0) (map/cps (lambda (v1) (k (cons v0 v1))) fn (cdr xs))) (car xs))))
        if (list is List.NonEmptyList properList) {
            Continuation.OneArgDelegate k2 = (v0) => map_internal((Continuation.OneArgDelegate)((v1) => k((Expr)Expr.Pair.Cons(v0, (List) v1))), proc, (List)properList.Cdr);
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
        // TODO: make an applyinternalcontinuation method in continuationExpr
            if (k is Continuation.ContinuationAny cany) {
                cany(arg);
                return;
            }
            if (k is Continuation.OneArgDelegate c1) {
                c1(arg);
                return;
            }
            k.DynamicInvoke(arg);
            return;
    }

    public static void values(Delegate k, List args) {
        new Continuation(k).Apply(args);
    }

    public static void callcc(Delegate k, List args) {
        if (args.Count() != 1) throw new Exception($"call/cc: expected one argument.");
        LiteralExpr<Delegate> proc = args.ElementAt(0) as LiteralExpr<Delegate> ?? throw new Exception("call/cc: expected procedure argument but got {args.ElementAt(0)}");
        if (proc.Value is Action<Delegate, Expr> del) {
            del(k, new Continuation(k));
            return;
        } else {
            throw new Exception("call/cc: expected procedure with one parameter but got {proc}");
        }
        // apply(k, proc, List.NewList(new Continuation(k)));
        // return;
    }

    public static void apply (Delegate k, Expr proc, List args) {
        if (proc is Continuation ce) {
            ce.Apply(args);
            return;
        }
        LiteralExpr<Delegate> del = proc as LiteralExpr<Delegate> ?? throw new Exception($"apply: expected procedure as first argument, but got {proc}");
        switch (del.Value) {
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
