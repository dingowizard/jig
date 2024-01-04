namespace Jig;

public delegate void Builtin(Delegate k, List args);

internal static class Builtins {

    internal static void map_internal(Continuation.OneArgDelegate k, Action<Continuation.OneArgDelegate, LiteralExpr<CompiledCode>> proc, List list) {
// // (define (map/cps k fn xs)
// //   (if (null? xs)
// //       (k xs)
// //       (fn (lambda (v0) (map/cps (lambda (v1) (k (cons v0 v1))) fn (cdr xs))) (car xs))))
        if (list is List.NonEmpty properList) {
            Continuation.OneArgDelegate k2 = (v0) => map_internal((Continuation.OneArgDelegate)((v1) => k((Expr)Expr.Pair.Cons(v0, (List) v1))), proc, (List)properList.Cdr);
            proc(k2, (LiteralExpr<CompiledCode>)properList.Car);
            return;
        } else {
            k(list); // at this point , k is (lambda (l) (apply (car l) (cdr l)))
            return; // TODO: why is this needed?
        }
    }

    public static void car (Delegate k, List args) {
        if (args is List.NonEmpty properList) {
            switch (properList.Car) {
                case IPair pair:
                    Continuation.ApplyDelegate(k, pair.Car);
                    return;
                default:
                    throw new Exception($"car: expected pair but got {properList.Car}");
            }
        } else {
            throw new Exception("car: expected one argument but got none");
        }
    }

    public static void nullP (Delegate k, List args) {
        if (args is List.NonEmpty properList) {
            Expr arg = properList.Car;
            if (arg is Expr.NullType) {
                Continuation.ApplyDelegate(k, new Expr.Boolean(true));
                return;
            }
            Continuation.ApplyDelegate(k, new Expr.Boolean(false));
            return;

        } else {
            throw new Exception("null?: expected one argument but got none");
        }
    }

    public static void cdr (Delegate k, List args) {
        if (args is List.NonEmpty properList) {
            switch (properList.Car) {
                case IPair pair:
                    Continuation.ApplyDelegate(k, pair.Cdr);
                    return;
                default:
                    throw new Exception($"cdr: expected pair but got {properList.Cdr}");
            }
        } else {
            throw new Exception("cdr: expected one argument but got none");
        }
    }

    public static void succ(Delegate k, List args) {
        if (args is List.NonEmpty properList) {
            object arg = properList.Car;
            if (arg is Expr.Integer intExpr) {
                Continuation.ApplyDelegate(k, new Expr.Integer(intExpr.Value + 1));
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
        Continuation.ApplyDelegate(k, resultIsInt ? intResult : doubleResult);
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
        Continuation.ApplyDelegate(k, resultIsInt ? intResult : doubleResult);
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
                        Continuation.ApplyDelegate(k, new Expr.Boolean(false));
                        return;
                    }
                } else if (arg is Expr.Double) {
                    Continuation.ApplyDelegate(k, new Expr.Boolean(false));
                    return;
                } else {
                    throw new Exception($"=: expected numeric arguments, but got {arg}");
                }
            }
            Continuation.ApplyDelegate(k, new Expr.Boolean(true));
            return;
        } else if (first is Expr.Double d) {
            Expr.Double current = d;
            foreach(var arg in args) {
                if (arg is Expr.Double nextD) {
                    if (current.Equals(nextD)) {
                        continue;
                    } else {
                        Continuation.ApplyDelegate(k, new Expr.Boolean(false));
                        return;
                    }
                } else if (arg is Expr.Integer) {
                    Continuation.ApplyDelegate(k, new Expr.Boolean(false));
                    return;
                } else {
                    throw new Exception($"=: expected numeric arguments, but got {arg}");
                }
            }
            Continuation.ApplyDelegate(k, new Expr.Boolean(true));
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
        Continuation.ApplyDelegate(k, resultIsInt ? intResult : doubleResult);
        return;
    }

    public static void cons (Delegate k, List args) {
        if (args is List.NonEmpty properList) {
            if (properList.Count() != 2) {
                throw new Exception("cons: expected two arguments");
            }
            Expr car = args.ElementAt(0);
            Expr cdr = args.ElementAt(1);
            Expr result = (Expr)Expr.Pair.Cons(car, cdr);
            Continuation.ApplyDelegate(k, result);
            return;
        } else {
            throw new Exception("cons: expected two arguments but got none");
        }
    }


    public static void values(Delegate k, List args) {
        new Continuation(k).Apply(args);
    }

    public static void callcc(Delegate k, List args) {
        if (args.Count() != 1) throw new Exception($"call/cc: expected one argument.");
        Procedure proc = args.ElementAt(0) as Procedure ?? throw new Exception("call/cc: expected procedure argument but got {args.ElementAt(0)}");
        if (proc.Value is Action<Delegate, Expr> del) {
            del(k, new Continuation(k));
            return;
        } else {
            throw new Exception("call/cc: expected procedure with one parameter but got {proc}");
        }
    }

    public static void dynamic_wind(Delegate k, List args) {}

    public static void apply (Delegate k, Expr x, List args) {
        if (x is Continuation cont) {
            cont.Apply(args);
            return;
        } else if (x is Procedure proc) {
            proc.Apply(k, args);
            return;
        } else {
            throw new Exception($"apply: expected procedure as first argument, but got {x}");
        }

    }

    public static void error(Delegate k, List args) {
        // TODO: something with second argument
        if (args.Count() < 2) {
            error(k, List.NewList(new Expr.String($"error: expected at least two arguments"), List.Empty));
            return;
        }
        Expr.String? msg = args.ElementAt(0) as Expr.String;
        if (msg is null) {
            error(k, List.NewList(new Expr.String($"error: expected first argument to be a string but got {args.ElementAt(0).Print()}"), List.Empty));
            return;
        }
        Console.Error.WriteLine(msg.Value);
        return;
    }

}
