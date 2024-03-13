namespace Jig;

public delegate Thunk Builtin(Delegate k, List args);

internal static class Builtins {

    internal static Thunk map_internal(Continuation.OneArgDelegate k, Func<Continuation.OneArgDelegate, LiteralExpr<CompiledCode>, Thunk> proc, List list) {
// // (define (map/cps k fn xs)
// //   (if (null? xs)
// //       (k xs)
// //       (fn (lambda (v0) (map/cps (lambda (v1) (k (cons v0 v1))) fn (cdr xs))) (car xs))))
        if (list is List.NonEmpty properList) {
            List? args = properList.Cdr as List;
            if (args is null) {
                throw new Exception($"in Builtins.map_internal: tried to get cdr of {properList} as List but type was {properList.Cdr.GetType()}");
            }
            Continuation.OneArgDelegate k2 = (v0) => map_internal((Continuation.OneArgDelegate)((v1) => k((Expr)Expr.Pair.Cons(v0, (List) v1))), proc, args);
            return proc(k2, (LiteralExpr<CompiledCode>)properList.Car);
        } else {
            return k(list); // at this point , k is (lambda (l) (apply (car l) (cdr l)))
        }
    }

    public static Thunk car (Delegate k, List args) {
        if (args is List.NonEmpty properList) {
            switch (properList.Car) {
                case IPair pair:
                    return Continuation.ApplyDelegate(k, pair.Car);
                default:
                    throw new Exception($"car: expected pair but got {properList.Car}");
            }
        } else {
            throw new Exception("car: expected one argument but got none");
        }
    }

    public static  Thunk nullP (Delegate k, List args) {
        if (args is List.NonEmpty properList) {
            Expr arg = properList.Car;
            if (arg is Expr.NullType) {
                return Continuation.ApplyDelegate(k, new Expr.Boolean(true));
            }
            return Continuation.ApplyDelegate(k, new Expr.Boolean(false));

        } else {
            throw new Exception("null?: expected one argument but got none");
        }
    }

    public static Thunk cdr (Delegate k, List args) {
        if (args is List.NonEmpty properList) {
            switch (properList.Car) {
                case IPair pair:
                    return Continuation.ApplyDelegate(k, pair.Cdr);
                default:
                    throw new Exception($"cdr: expected pair but got {properList.Cdr}");
            }
        } else {
            throw new Exception("cdr: expected one argument but got none");
        }
    }

    public static Thunk succ(Delegate k, List args) {
        if (args is List.NonEmpty properList) {
            object arg = properList.Car;
            if (arg is Expr.Integer intExpr) {
                return Continuation.ApplyDelegate(k, new Expr.Integer(intExpr.Value + 1));
            } else {
                throw new Exception($"incr: expected single integer argument, but got {args}");
            }
        } else {
            throw new Exception("succ: expected one argument but got none");
        }
    }

    public static Thunk sum(Delegate k, List args) {
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
        return Continuation.ApplyDelegate(k, resultIsInt ? intResult : doubleResult);

    }

    public static Thunk diff(Delegate k, Expr first, List args) {
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
        return Continuation.ApplyDelegate(k, resultIsInt ? intResult : doubleResult);
    }

    public static Thunk numEq(Delegate k, Expr first, List args) {
        // TODO: should 1 = 1.0? it does in chez. if so best way to implement
        if (first is Expr.Integer i) {
            Expr.Integer current = i;
            foreach(var arg in args) {
                if (arg is Expr.Integer nextInt) {
                    if (current.Equals(nextInt)) {
                        continue;
                    } else {
                        return Continuation.ApplyDelegate(k, new Expr.Boolean(false));
                    }
                } else if (arg is Expr.Double) {
                    return Continuation.ApplyDelegate(k, new Expr.Boolean(false));
                } else {
                    throw new Exception($"=: expected numeric arguments, but got {arg}");
                }
            }
            return Continuation.ApplyDelegate(k, new Expr.Boolean(true));
        } else if (first is Expr.Double d) {
            Expr.Double current = d;
            foreach(var arg in args) {
                if (arg is Expr.Double nextD) {
                    if (current.Equals(nextD)) {
                        continue;
                    } else {
                        return Continuation.ApplyDelegate(k, new Expr.Boolean(false));
                    }
                } else if (arg is Expr.Integer) {
                    return Continuation.ApplyDelegate(k, new Expr.Boolean(false));
                } else {
                    throw new Exception($"=: expected numeric arguments, but got {arg}");
                }
            }
            return Continuation.ApplyDelegate(k, new Expr.Boolean(true));

        } else {
            throw new Exception($"=: expects only numeric arguments. Got {first}");
        }
    }

    public static Thunk product(Delegate k, List args) {
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
        return Continuation.ApplyDelegate(k, resultIsInt ? intResult : doubleResult);
    }

    public static Thunk cons (Delegate k, List args) {
        if (args is List.NonEmpty properList) {
            if (properList.Count() != 2) {
                throw new Exception("cons: expected two arguments");
            }
            Expr car = args.ElementAt(0);
            Expr cdr = args.ElementAt(1);
            Expr result = (Expr)Expr.Pair.Cons(car, cdr);
            return Continuation.ApplyDelegate(k, result);
        } else {
            throw new Exception("cons: expected two arguments but got none");
        }
    }


    public static Thunk values(Delegate k, List args) {
        return new Continuation(k).Apply(args);
    }

    public static Thunk callcc(Delegate k, List args) {
        if (args.Count() != 1) throw new Exception($"call/cc: expected one argument.");
        Procedure proc = args.ElementAt(0) as Procedure ?? throw new Exception("call/cc: expected procedure argument but got {args.ElementAt(0)}");
        if (proc.Value is Func<Delegate, Expr, Thunk> del) {
            return del(k, new Continuation(k));
            // return Continuation.ApplyDelegate(k, del(k, new Continuation(k)));
            // Continuation.Thunk thunk = () => del(k, new Continuation(k));
            // // return new Continuation.MaybeThunk.Thunk(thunk);
            // return proc.Apply(k, List.NewList(new Continuation(k)));
        } else {
            throw new Exception("call/cc: expected procedure with one parameter but got {proc}");
        }
    }

    // public static void dynamic_wind(Delegate k, List args) {}

    public static Thunk apply (Delegate k, Expr x, List args) {
        // Console.WriteLine($"in Builtins.apply: applying {x} to {args}");
        if (x is Continuation cont) {
            return cont.Apply(args);
        } else if (x is Procedure proc) {
            return proc.Apply(k, args);
        } else {
            throw new Exception($"apply: expected procedure as first argument, but got {x}");
        }
    }

    public static Thunk syntax_to_list(Delegate k, List args) {
        if (args.Count() != 1) throw new Exception($"syntax->list: expected one argument.");
        Syntax stx = args.ElementAt(0) as Syntax ?? throw new Exception($"syntax->list: expected a syntax argument, got got {args.ElementAt(0)}");
        if (Syntax.ToList(stx, out SyntaxList? result)) {
            return Continuation.ApplyDelegate(k, result);
        } else {
            return Continuation.ApplyDelegate(k, new Expr.Boolean(false));
        }
    }

    public static Thunk syntax_p(Delegate k, List args) {
        if (args.Count() != 1) throw new Exception($"syntax?: expected one argument.");
        if (args.ElementAt(0) is Syntax) {
            return Continuation.ApplyDelegate(k, new Expr.Boolean(true));
        } else {
            return Continuation.ApplyDelegate(k, new Expr.Boolean(false));
        }
    }

    public static Thunk pair_p(Delegate k, List args) {
        if (args.Count() != 1) throw new Exception($"pair?: expected one argument.");
        return Continuation.ApplyDelegate(k, new Expr.Boolean(args.ElementAt(0) is IPair));
    }

    public static Thunk datum_to_syntax(Delegate k, List args) {
        if (args.Count() != 2) throw new Exception($"datum->syntax: expected two arguments but got {args.Count()}.");
        if (args.ElementAt(0) is Syntax stx) {
            return Continuation.ApplyDelegate(k, Syntax.FromDatum(stx.SrcLoc, args.ElementAt(1)));
        } else {
            throw new Exception($"datum->syntax: expected first argument to be syntax, but got {args.ElementAt(0)}");
        }
    }

    public static Thunk expand_once(Delegate k, List args) {
        // TODO: this doesn't work as intended because the macroexpander expands the argument before the function gets it
        // needs to be a macro?
        if (args.Count() != 1) throw new Exception($"expand-once: expected a single argument but got {args.Count()}");
        if (args.ElementAt(0) is Syntax stx) {
            // TODO: what should expansion environment be?
            (_, Syntax result) = MacroExpander.Expand_1(stx, ExpansionEnvironment.Default);
            return Continuation.ApplyDelegate(k, result);
        } else {
            throw new Exception($"expand-once: expected syntax argument, but got {args.ElementAt(0)}");
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
