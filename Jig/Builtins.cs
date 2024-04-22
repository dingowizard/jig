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
                    return Error(k, $"car: expected pair but got {properList.Car}");
            }
        } else {
            return Error(k, "car: expected one argument but got none");
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
            return Error(k, "null?: expected one argument but got none");
        }
    }

    public static Thunk cdr (Delegate k, List args) {
        if (args is List.NonEmpty properList) {
            switch (properList.Car) {
                case IPair pair:
                    return Continuation.ApplyDelegate(k, pair.Cdr);
                default:
                    return Error(k, $"cdr: expected pair but got {properList.Cdr}");
            }
        } else {
            return Error(k, "cdr: expected one argument but got none");
        }
    }

    public static Thunk char_p(Delegate k, List args) {
        if (args is List.NonEmpty properList) {
            if (args.Count() != 1) return Error(k, "char?: expected one argument but got {args.Count()}");
            Expr arg = properList.Car;
            if (arg is Expr.Char) {
                return Continuation.ApplyDelegate(k, new Expr.Boolean(true));
            }
            return Continuation.ApplyDelegate(k, new Expr.Boolean(false));

        } else {
            return Error(k, "char?: expected one argument but got none");
        }
    }

    public static Thunk succ(Delegate k, List args) {
        if (args is List.NonEmpty properList) {
            object arg = properList.Car;
            if (arg is Expr.Integer intExpr) {
                return Continuation.ApplyDelegate(k, new Expr.Integer(intExpr.Value + 1));
            } else {
                return Error(k, $"succ: expected single integer argument, but got {args}");
            }
        } else {
            return Error(k, "succ: expected one argument but got none");
        }
    }

    public static Thunk number_p(Delegate k, List args) {
        if (args is List.NonEmpty properList) {
            object arg = properList.Car;
            if (arg is Expr.Integer intExpr) {
                return Continuation.ApplyDelegate(k, new Expr.Boolean(true));
            } else if (arg is Expr.Double) {
                return Continuation.ApplyDelegate(k, new Expr.Boolean(true));
            } else {
                return Continuation.ApplyDelegate(k, new Expr.Boolean(false));
            }
        } else {
            return Error(k, "number?: expected one argument but got none");
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
                    return Error(k, $"+: all args must be numbers. {arg} is not a number.");
                }
            } else {
                if (arg is Expr.Integer i) {
                    doubleResult += i;
                    continue;
                } else if (arg is Expr.Double d) {
                    doubleResult += d;
                    continue;
                } else {
                    return Error(k, $"+: all args must be numbers. {arg} is not a number.");
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
            return Error(k, $"-: all args must be numbers. {first} is not a number.");
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
                    return Error(k, $"-: all args must be numbers. {arg} is not a number.");
                }
            } else {
                if (arg is Expr.Integer x) {
                    doubleResult -= x;
                    continue;
                } else if (arg is Expr.Double d) {
                    doubleResult -= d;
                    continue;
                } else {
                    return Error(k, $"-: all args must be numbers. {arg} is not a number.");
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
                    return Error(k, $"=: expected numeric arguments, but got {arg}");
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
                    return Error(k, $"=: expected numeric arguments, but got {arg}");
                }
            }
            return Continuation.ApplyDelegate(k, new Expr.Boolean(true));

        } else {
            return Error(k, $"=: expects only numeric arguments. Got {first}");
        }
    }

    public static Thunk gt(Delegate k, List args) {
        // TODO: OMG this is hideous. Move some of this logic into a Number class
        // TODO: write some tests
        if (args is List.NonEmpty nonEmpty) {
            Expr first = nonEmpty.ElementAt(0);
            args = nonEmpty.Rest;
            while (args is List.NonEmpty rest) {
                Expr second = rest.ElementAt(0);
                if (first is Expr.Double d1) {
                    if (second is Expr.Integer i2) {
                        if (d1.Value > i2.Value) {
                            first = second;
                            args = rest.Rest;
                            continue;
                        } else {
                            return Continuation.ApplyDelegate(k, new Expr.Boolean(false));
                        }
                    } else if (second is Expr.Double d2) {
                        if (d1.Value > d2.Value) {
                            first = second;
                            args = rest.Rest;
                            continue;
                        } else {
                            return Continuation.ApplyDelegate(k, new Expr.Boolean(false));
                        }
                    } else {
                        return Error(k, $">: expected arguments to be numbers but got {first}");
                    }
                } else if (first is Expr.Integer i1) {
                    if (second is Expr.Integer i2) {
                        if (i1.Value > i2.Value) {
                            first = second;
                            args = rest.Rest;
                            continue;
                        } else {
                            return Continuation.ApplyDelegate(k, new Expr.Boolean(false));
                        }
                    } else if (second is Expr.Double d2) {
                        if (i1.Value > d2.Value) {
                            first = second;
                            args = rest.Rest;
                            continue;
                        } else {
                            return Continuation.ApplyDelegate(k, new Expr.Boolean(false));
                        }

                    } else {
                        return Error(k, $">: expected arguments to be numbers but got {first}");
                    }
                } else {
                    return Error(k, $">: expected arguments to be numbers but got {first}");
                }
            }
            return Continuation.ApplyDelegate(k, new Expr.Boolean(true));
        } else {
            return Error(k, ">: expected at least one argument but got none");
        }
    }

    public static Thunk eq_p(Delegate k, List args) {
        // TODO: write some tests for eq?
        if (args is List.NonEmpty properList) {
            if (properList.Count() != 2) {
                return Error(k, "eq?: expected two arguments");
            }
            Expr first = args.ElementAt(0);
            Expr second = args.ElementAt(1);
            bool result = first is IPair pair ? Object.ReferenceEquals(first,second) : first.Equals(second);
            return Continuation.ApplyDelegate(k, new Expr.Boolean(result));
        } else {
            return Error(k, "eq?: expected two arguments but got none");
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
                    return Error(k, $"*: all args must be numbers. {arg} is not a number.");
                }
            } else {
                if (arg is Expr.Integer i) {
                    doubleResult *= i;
                    continue;
                } else if (arg is Expr.Double d) {
                    doubleResult *= d;
                    continue;
                } else {
                    return Error(k, $"*: all args must be numbers. {arg} is not a number.");
                }

            }
        }
        return Continuation.ApplyDelegate(k, resultIsInt ? intResult : doubleResult);
    }

    public static Thunk cons (Delegate k, List args) {
        if (args is List.NonEmpty properList) {
            if (properList.Count() != 2) {
                return Error(k, "cons: expected two arguments");
            }
            Expr car = args.ElementAt(0);
            Expr cdr = args.ElementAt(1);
            Expr result = (Expr)Expr.Pair.Cons(car, cdr);
            return Continuation.ApplyDelegate(k, result);
        } else {
            return Error(k, "cons: expected two arguments but got none");
        }
    }

    public static Thunk symbol_equal_p (Delegate k, List args) {
        if (args is List.NonEmpty properList) {
            if (properList.Count() < 2) {
                return Error(k, "symbol=?: expected two or more arguments.");
            }
            Expr.Symbol? sym1 = properList.Car as Expr.Symbol;
            if (sym1 is null) return Error(k, $"symbol=?: expected all arguments to be symbols, but got {properList.Car}");
            List rest = properList.Rest;
            Expr.Symbol? sym2 = rest.ElementAt(0) as Expr.Symbol;
            if (sym2 is null) return Error(k, $"symbol=?: expected all arguments to be symbols, but got {rest.ElementAt(0)}");
            while (rest is List.NonEmpty nonEmpty) {
                if (!sym1.Equals(sym2)) {
                    return Continuation.ApplyDelegate(k, new Expr.Boolean(false));
                }
                sym1 = sym2;
                sym2 = nonEmpty.Car as Expr.Symbol;
                if (sym2 is null) return Error(k, $"symbol=?: expected all arguments to be symbols, but got {nonEmpty.Car}");
                rest = nonEmpty.Rest;
            }
            if (sym1.Equals(sym2)) {
                return Continuation.ApplyDelegate(k, new Expr.Boolean(true));
            } else {
                return Continuation.ApplyDelegate(k, new Expr.Boolean(false));
            }
        } else {
            return Error(k, "symbol=?: expected two or more arguments.");
        }

    }

    public static Thunk symbol_p (Delegate k, List args) {
        if (args is List.NonEmpty properList) {
            if (properList.Count() != 1) {
                return Error(k, "symbol?: expected one argument.");
            }
            return Continuation.ApplyDelegate(k, new Expr.Boolean(properList.Car is Expr.Symbol));
        } else {
            return Error(k, "symbol?: expected one argument.");
        }

    }

    public static Thunk syntax_e (Delegate k, List args) {
        if (args is List.NonEmpty properList) {
            if (properList.Count() != 1) {
                return Error(k, "syntax-e: expected one argument.");
            }
            Syntax? stx = properList.Car as Syntax;
            if (stx is null) return Error(k, $"syntax-e: expected syntax argument but got {properList.Car}");
            return Continuation.ApplyDelegate(k, Syntax.E(stx));
        } else {
            return Error(k, "syntax-e: expected one argument.");
        }

    }

    public static Thunk symbol_to_string (Delegate k, List args) {
        if (args is List.NonEmpty properList) {
            if (properList.Count() != 1) {
                return Error(k, "symbol->string: expected one argument.");
            }
            if (properList.Car is Expr.Symbol symbol) {
                return Continuation.ApplyDelegate(k, new Expr.String(symbol.Name));
            } else {
                return Error(k, "symbol->string: expected its argument to be a symbol.");
            }
        } else {
            return Error(k, "symbol->string: expected one argument.");
        }

    }

    public static Thunk string_to_symbol (Delegate k, List args) {
        if (args is List.NonEmpty properList) {
            if (properList.Count() != 1) {
                return Error(k, "string->symbol: expected one argument.");
            }
            if (properList.Car is Expr.String str) {
                return Continuation.ApplyDelegate(k, new Expr.Symbol(str.Value));
            } else {
                return Error(k, "string->symbol: expected its argument to be a string.");
            }
        } else {
            return Error(k, "string->symbol: expected one argument.");
        }

    }

    public static Thunk display(Delegate k, List args) {
        // TODO: should take argument for port but use current port parameter when no arg
        // TODO: should write strings like 'write-string' : no quotes for one thing
        if (args is List.NonEmpty properList) {
            if (properList.Count() > 2) {
                return Error(k, "display: expected one or two but not {properList.Count()} arguments.");
            }
            Console.Write(properList.Car.Print());
            return Continuation.ApplyDelegate(k, Expr.Void);
        } else {
            return Error(k, "display: expected at least one argument.");
        }
    }

    public static Thunk newline(Delegate k, List args) {
        // TODO: should take argument for port but use current port parameter when no arg
        if (args is List.NonEmpty properList) {
            if (properList.Count() > 1) {
                return Error(k, "newline: expected zero or one arguments but not {properList.Count()}.");
            }
            Console.WriteLine("");
            return Continuation.ApplyDelegate(k, Expr.Void);
        } else {
            Console.WriteLine("");
            return Continuation.ApplyDelegate(k, Expr.Void);
        }
    }

    public static Thunk values(Delegate k, List args) {
        return new Continuation(k).Apply(args);
    }

    public static Thunk callcc(Delegate k, List args) {
        if (args.Count() != 1) return Error(k, $"call/cc: expected one argument.");
        Procedure? proc = args.ElementAt(0) as Procedure;
        if (proc is null) return Error(k, "call/cc: expected procedure argument but got {args.ElementAt(0)}");
        if (proc.Value is Func<Delegate, Expr, Thunk> del) {
            return del(k, new Continuation(k));
        } else {
            return Error(k, "call/cc: expected procedure with one parameter but got {proc}");
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
            return Error(k, $"apply: expected procedure as first argument, but got {x}");
        }
    }

    public static Thunk syntax_to_list(Delegate k, List args) {
        if (args.Count() != 1) return Error(k, $"syntax->list: expected one argument.");
        Syntax? stx = args.ElementAt(0) as Syntax;
        if (stx is null) return Error(k, $"syntax->list: expected a syntax argument, got got {args.ElementAt(0)}");
        if (Syntax.ToList(stx, out List? result)) {
            return Continuation.ApplyDelegate(k, result);
        } else {
            return Continuation.ApplyDelegate(k, new Expr.Boolean(false));
        }
    }

    public static Thunk syntax_p(Delegate k, List args) {
        if (args.Count() != 1) return Error(k, $"syntax?: expected one argument.");
        if (args.ElementAt(0) is Syntax) {
            return Continuation.ApplyDelegate(k, new Expr.Boolean(true));
        } else {
            return Continuation.ApplyDelegate(k, new Expr.Boolean(false));
        }
    }

    public static Thunk pair_p(Delegate k, List args) {
        if (args.Count() != 1) return Error(k, $"pair?: expected one argument.");
        return Continuation.ApplyDelegate(k, new Expr.Boolean(args.ElementAt(0) is IPair));
    }

    public static Thunk datum_to_syntax(Delegate k, List args) {
        if (args.Count() != 2) return Error(k, $"datum->syntax: expected two arguments but got {args.Count()}.");
        if (args.ElementAt(0) is Syntax stx) {
            return Continuation.ApplyDelegate(k, Syntax.FromDatum(stx.SrcLoc, args.ElementAt(1)));
        } else {
            return Error(k, $"datum->syntax: expected first argument to be syntax, but got {args.ElementAt(0)}");
        }
    }

    public static Thunk expand(Delegate k, List args) {
        if (args.Count() != 1) return Error(k, $"expand: expected a single argument but got {args.Count()}");
        if (args.ElementAt(0) is Syntax stx) {
            // TODO: what should expansion environment be?
            Syntax result = new MacroExpander().Expand(stx, ExpansionEnvironment.Default);
            return Continuation.ApplyDelegate(k, result);
        } else {
            stx = Syntax.FromDatum(new SrcLoc(), args.ElementAt(0));
            Syntax result = new MacroExpander().Expand(stx, ExpansionEnvironment.Default);
            return Continuation.ApplyDelegate(k, result);
        }
    }

    public static Thunk expand_once(Delegate k, List args) {
        if (args.Count() != 1) return Error(k, $"expand-once: expected a single argument but got {args.Count()}");
        if (args.ElementAt(0) is Syntax stx) {
            // TODO: what should expansion environment be?
            Syntax result = new MacroExpander().Expand(stx, ExpansionEnvironment.Default, once: true);
            return Continuation.ApplyDelegate(k, result);
        } else {
            stx = Syntax.FromDatum(new SrcLoc(), args.ElementAt(0));
            Syntax result = new MacroExpander().Expand(stx, ExpansionEnvironment.Default, once: true);
            return Continuation.ApplyDelegate(k, result);
        }
    }

    internal static Thunk Error(Delegate k, string msg, params Expr[] rest) {
        // the reason we have to look up "error" in the global environment is that
        // in prelude we redefine error so that it runs the 'out' thunks in winders.
        // otherwise *current-exception-handlers* isn't restored properly
        //
        // TODO: should we use raise or raise-continuable in builtins instead?
        // TODO: cache this search somehow
        Expr errorExpr = Program.TopLevel[new Expr.Symbol("error")];
        if (errorExpr is Procedure proc) {
            return proc.Apply(k, List.NewList(new Expr.String(msg)));
        } else {
            throw new Exception("error is not bound to a procedure");
        }
    }

    public static Thunk error(Delegate k, List args) {
        // TODO: should probably be possible to use a different continuation than id
        // for example if we are reading in a whole script rather than an expr at REPL
        // TODO: print other args
        // TODO: stack trace
        Console.Error.Write("*** ERROR: ");
        Continuation.OneArgDelegate end = (x) => null;
        if (args.Count() < 1) {
            Console.Error.WriteLine("error: expected at least one argument");
            return Continuation.ApplyDelegate(end, Expr.Void);
        }
        Expr.String? msg = args.ElementAt(0) as Expr.String;
        if (msg is null) {
            Console.Error.WriteLine($"error: expected first argument to be a string message but got {args.ElementAt(0)}.");
        } else {
            Console.Error.WriteLine(msg);
        }
        return Continuation.ApplyDelegate(end, Expr.Void);
    }

    public static Thunk vector(Delegate k, List args) {
        return Continuation.ApplyDelegate(k, new Expr.Vector(args));

    }

    public static Thunk vector_length(Delegate k, List args) {
        if (args.Count() != 1) return Error(k, $"vector-length: expected a single argument but got {args.Count()}");
        if (args.ElementAt(0) is Expr.Vector v) {
            return Continuation.ApplyDelegate(k, v.Length);
        } else {
            return Error(k, "vector-length: expected argument to be vector");

        }

    }

    public static Thunk vector_ref(Delegate k, List args) {
        if (args.Count() != 2) return Error(k, $"vector-ref: expected two arguments but got {args.Count()}");
        if (args.ElementAt(0) is Expr.Vector v) {
            if (args.ElementAt(1) is Expr.Integer i) {
                if (v.TryGetAtIndex(i, out Expr? result)) {
                    return Continuation.ApplyDelegate(k, result);
                } else {
                    return Error(k, $"vector-ref: {i} is not a valid index.");
                }

            } else {
                return Error(k, $"vector-ref: expected second argument to be an integer, but got {args.ElementAt(1)}");
            }
        } else {
            return Error(k, "vector-ref: expected first argument to be vector");

        }

    }

}
