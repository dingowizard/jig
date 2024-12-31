namespace Jig;

public delegate Thunk? Builtin(Delegate k, List args);

internal static partial class Builtins {


    internal static Thunk? map_internal(Continuation.OneArgDelegate k, IEnvironment env, CompiledCode[] codes, int index) {
// // (define (map/cps k fn xs)
// //   (if (null? xs)
// //       (k xs)
// //       (fn (lambda (arg) (map/cps (lambda (rest) (k (cons arg rest))) fn (cdr xs))) (car xs))))


        if (index < codes.Length) {
            Continuation.OneArgDelegate k2 = (arg) =>
                map_internal((Continuation.OneArgDelegate)((rest) =>
                    k(Pair.Cons(arg, (List) rest))), env, codes, index + 1);
            return codes[index](k2, env);
        } else {
            return k(List.Null);
        }
    }


    public static Thunk? car (Delegate k, List args) {
        if (args is INonEmptyList properList) {
            return properList.Car switch
            {
                IPair pair => Continuation.ApplyDelegate(k, pair.Car),
                _ => Error(k, $"car: expected pair but got {properList.Car.Print()}"),
            };
        } else {
            return Error(k, $"car: expected one argument but got {args}");
        }
    }


    public static Thunk? cdr (Delegate k, List args) {
        if (args is List.NonEmpty properList) {
            return properList.Car switch
            {
                IPair pair => Continuation.ApplyDelegate(k, pair.Cdr),
                _ => Error(k, $"cdr: expected pair but got {properList.Cdr}"),
            };
        } else {
            return Error(k, "cdr: expected one argument but got none");
        }
    }

    public static Thunk? map(Delegate k, List args) {
        // NOTE: making this builtin did not appreciably speed up tests
        if (args is List.NonEmpty properList) {
            if (properList.Car is not Procedure proc) {
                return Error(k, $"map: expected first argument to be a procedure, but got {properList.Car.Print()}");
            }

            if (properList.Cdr is not List.NonEmpty ls) {
                return Error(k, $"map: expected at least two arguments, but got {properList.Count()}");
            }

            if (ls.Car is not List xs) {
                return Error(k, $"map: expected second argument to be list, but got {ls.Car}");
                
            }

            var arrays = new System.Collections.Generic.List<IForm[]>() { xs.ToArray<IForm>() };

            foreach (var form in ls.Rest) {
                if (form is not List ys) {
                    return Error(k, $"map: expected list argument, but got {form}");
                }

                arrays.Add(ys.ToArray());
            }

            var length = arrays[0].Length;
            if (arrays.Skip(1).Any(arr => arr.Length != length)) {
                return Error(k, $"map: list arguments should all be the same length.");
            }
            var result = new IForm[length];
            for (var i = 0; i < length; i++) {
                var i1 = i;
                var v = proc.ApplyNonCPS( arrays.Select(a => a[i1]).ToJigList());
                result[i] = v;

            }

            return Continuation.ApplyDelegate(k, result.ToJigList());


        } else {
            return Error(k, "map: expected one argument but got none");
        }
        
    }

    public static  Thunk? nullP (Delegate k, List args) {
        if (args is INonEmptyList properList) {
            IForm arg = properList.Car;
            if (arg is IList list) {
                return Continuation.ApplyDelegate(k, list.NullP);
            }
            return Continuation.ApplyDelegate(k, Bool.False);

        } else {
            return Error(k, "null?: expected one argument but got none");
        }
    }
    public static Thunk? char_p(Delegate k, List args) {
        if (args is List.NonEmpty properList) {
            if (args.Count() != 1) return Error(k, "char?: expected one argument but got {args.Count()}");
            IForm arg = properList.Car;
            if (arg is Char) {
                return Continuation.ApplyDelegate(k, Bool.True);
            }
            return Continuation.ApplyDelegate(k, Bool.False);

        } else {
            return Error(k, "char?: expected one argument but got none");
        }
    }

    public static Thunk? succ(Delegate k, List args) {
        if (args is List.NonEmpty properList) {
            object arg = properList.Car;
            if (arg is Integer intExpr) {
                return Continuation.ApplyDelegate(k, new Integer(intExpr.Value + 1));
            } else {
                return Error(k, $"succ: expected single integer argument, but got {args}");
            }
        } else {
            return Error(k, "succ: expected one argument but got none");
        }
    }

    public static Thunk? number_p(Delegate k, List args) {
        if (args is List.NonEmpty properList) {
            if (properList.Count() == 1) {
                return Continuation.ApplyDelegate(k, properList.Car is Number ? Bool.True : Bool.False);
            } else {
                return Error(k, $"number?: expected one argument and only one argument. Got {args}.");
            }
        } else {
            return Error(k, "number?: expected one argument but got none");
        }
    }

    public static Thunk? sum(Delegate k, List args) {
        Number acc = Number.From(0);
        foreach (var arg in args) {
            if (arg is Number num) {
                acc += num;
            } else {
                return Error(k, $"+: all args must be numbers. got {arg}");
            }
        }
        return Continuation.ApplyDelegate(k, acc);
    }


    public static Thunk? numEq(Delegate k, IForm first, List args) {
        if (first is Number n1) {
            foreach (var arg in args) {
                if (arg is Number n2) {
                    Bool b = n1 == n2;
                    if (b.Value) {
                        continue;
                    }
                    return Continuation.ApplyDelegate(k, b);
                } else {
                    return Error(k, $"=: expects only numeric arguments. Got {arg}");
                }
            }
            return Continuation.ApplyDelegate(k, Bool.True);
        } else {
            return Error(k, $"=: expects only numeric arguments. Got {first}");
        }

    }


    public static Thunk? eq_p(Delegate k, List args) {
        // TODO: write some tests for eq?
        if (args is List.NonEmpty properList) {
            if (properList.Count() != 2) {
                return Error(k, "eq?: expected two arguments");
            }
            IForm first = args.ElementAt(0);
            IForm second = args.ElementAt(1);
            bool result = first is IPair ? Object.ReferenceEquals(first, second) : first.Equals(second);
            return Continuation.ApplyDelegate(k, result ? Bool.True : Bool.False);
        } else {
            return Error(k, "eq?: expected two arguments but got none");
        }
    }

    public static Thunk? eqv_p(Delegate k, List args) {
        // TODO: write some tests for eqv?
        if (args is INonEmptyList properList) {
            if (!properList.Length.Equals(Integer.Two)) {
                return Error(k, "eqv?: expected two arguments");
            }
            IForm first = args.ElementAt(0);
            IForm second = args.ElementAt(1);
            // Console.WriteLine($"eqv?: testing {first.Print()}, a {first.GetType()}, against {second.Print()}, a {second.GetType()}");
            bool result = first.Equals(second);
            return Continuation.ApplyDelegate(k, result ? Bool.True : Bool.False);
        } else {
            return Error(k, "eqv?: expected two arguments but got none");
        }
    }


    public static Thunk? cons (Delegate k, List args) {
        if (args is List.NonEmpty properList) {
            if (properList.Count() != 2) {
                return Error(k, "cons: expected two arguments");
            }
            IForm car = args.ElementAt(0);
            IForm cdr = args.ElementAt(1);
            IPair result = Pair.Cons(car, cdr);
            return Continuation.ApplyDelegate(k, result);
        } else {
            return Error(k, "cons: expected two arguments but got none");
        }
    }

    public static Thunk? append(Delegate k, IList args) {
        // TODO: this is inefficient because Append needs to run through the accumulator every time it appends another list
        if (args is INonEmptyList xs) {
            IForm result = xs.Car;
            IList rest = xs.Rest;
            while (rest is INonEmptyList properRest) {
                if (result is IList firstList) {
                    IForm second = properRest.Car;
                    result = firstList.Append(second);
                    rest = properRest.Rest;
                } else {
                    if (xs.Rest is IEmptyList) {
                        return Continuation.ApplyDelegate(k, result);
                    } else {
                        return Error(k, $"append: {xs.Car} is not a proper list.");
                    }
                }
            }
            return Continuation.ApplyDelegate(k, result);
        }
        return Continuation.ApplyDelegate(k, List.Null);

    }

    public static Thunk? symbol_equal_p (Delegate k, List args) {
        if (args is List.NonEmpty properList) {
            if (properList.Count() < 2) {
                return Error(k, "symbol=?: expected two or more arguments.");
            }
            if (properList.Car is not Form.Symbol sym1){
                return Error(k, $"symbol=?: expected all arguments to be symbols, but got {properList.Car}");
            }
            List rest = (List)properList.Rest;
            if (rest.ElementAt(0) is not Form.Symbol sym2) return Error(k, $"symbol=?: expected all arguments to be symbols, but got {rest.ElementAt(0)}");
            while (rest is List.NonEmpty nonEmpty) {
                if (!sym1.Equals(sym2)) {
                    return Continuation.ApplyDelegate(k, Bool.False);
                }
                sym1 = sym2;
                sym2 = nonEmpty.Car as Form.Symbol;
                if (sym2 is null) return Error(k, $"symbol=?: expected all arguments to be symbols, but got {nonEmpty.Car}");
                rest = (List)nonEmpty.Rest;
            }
            if (sym1.Equals(sym2)) {
                return Continuation.ApplyDelegate(k, Bool.True);
            } else {
                return Continuation.ApplyDelegate(k, Bool.False);
            }
        } else {
            return Error(k, "symbol=?: expected two or more arguments.");
        }

    }

    public static Thunk? symbol_p (Delegate k, List args) {
        if (args is List.NonEmpty properList) {
            if (properList.Count() != 1) {
                return Error(k, "symbol?: expected one argument.");
            }
            return Continuation.ApplyDelegate(k, properList.Car is Form.Symbol ? Bool.True : Bool.False);
        } else {
            return Error(k, "symbol?: expected one argument.");
        }

    }

    public static Thunk? syntax_e (Delegate k, List args) {
        if (args is INonEmptyList properList) {
            if (!properList.Length.Equals(Integer.One)) {
                return Error(k, $"syntax-e: expected one argument, but got {properList.Length.Print()}.");
            }
            if (properList.Car is not Syntax stx) {
                return Error(k, $"syntax-e: expected syntax argument but got {properList.Car}");
            }
            return Continuation.ApplyDelegate(k, Syntax.E(stx));
        } else {
            return Error(k, $"syntax-e: expected non-empty list of arguments. Got {args.Print()}, a {args.GetType()}.");
        }

    }

    public static Thunk? symbol_to_string (Delegate k, List args) {
        if (args is List.NonEmpty properList) {
            if (properList.Count() != 1) {
                return Error(k, "symbol->string: expected one argument.");
            }
            if (properList.Car is Form.Symbol symbol) {
                return Continuation.ApplyDelegate(k, new String(symbol.Name));
            } else {
                return Error(k, "symbol->string: expected its argument to be a symbol.");
            }
        } else {
            return Error(k, "symbol->string: expected one argument.");
        }

    }

    public static Thunk? string_to_symbol (Delegate k, List args) {
        if (args is List.NonEmpty properList) {
            if (properList.Count() != 1) {
                return Error(k, "string->symbol: expected one argument.");
            }
            if (properList.Car is String str) {
                return Continuation.ApplyDelegate(k, new Form.Symbol(str.Value));
            } else {
                return Error(k, "string->symbol: expected its argument to be a string.");
            }
        } else {
            return Error(k, "string->symbol: expected one argument.");
        }

    }

    public static Thunk? display(Delegate k, List args) {
        // TODO: should take argument for port but use current port parameter when no arg
        // TODO: should write strings like 'write-string' : no quotes for one thing
        if (args is List.NonEmpty properList) {
            if (properList.Count() > 2) {
                return Error(k, "display: expected one or two but not {properList.Count()} arguments.");
            }
            Console.Write(properList.Car.Print());
            return Continuation.ApplyDelegate(k, Form.Void);
        } else {
            return Error(k, "display: expected at least one argument.");
        }
    }

    public static Thunk? newline(Delegate k, List args) {
        // TODO: should take argument for port but use current port parameter when no arg
        if (args is List.NonEmpty properList) {
            if (properList.Count() > 1) {
                return Error(k, "newline: expected zero or one arguments but not {properList.Count()}.");
            }
            Console.WriteLine("");
            return Continuation.ApplyDelegate(k, Form.Void);
        } else {
            Console.WriteLine("");
            return Continuation.ApplyDelegate(k, Form.Void);
        }
    }

    public static Thunk? values(Delegate k, List args) {
        return new Continuation(k).Apply(args);
    }

    public static Thunk? callcc(Delegate k, List args) {
        if (args.Count() != 1) return Error(k, $"call/cc: expected one argument.");
        if (args.ElementAt(0) is not Procedure proc){
            return Error(k, "call/cc: expected procedure argument but got {args.ElementAt(0)}");
        }
        if (proc.Value is Func<Delegate, Form, Thunk> del) {
            return del(k, new Continuation(k));
        } else {
            return Error(k, "call/cc: expected procedure with one parameter but got {proc}");
        }
    }

    // public static void dynamic_wind(Delegate k, List args) {}

    public static Thunk? apply (Delegate k, IForm x, List args) {
        // Console.WriteLine($"in Builtins.apply: applying {x} to {args}");
        if (x is Continuation cont) {
            return cont.Apply(args);
        } else if (x is Procedure proc) {
            return proc.Apply(k, args);
        } else {
            return Error(k, $"apply: expected procedure as first argument, but got {x.Print()}");
        }
    }

    public static Thunk? syntax_to_list(Delegate k, List args) {
        if (args.Count() != 1) return Error(k, $"syntax->list: expected one argument.");
        if (args.ElementAt(0) is not Syntax stx){
            return Error(k, $"syntax->list: expected a syntax argument, got got {args.ElementAt(0)}");
        }
        if (Syntax.ToList(stx, out SyntaxList? result)) {
            return Continuation.ApplyDelegate(k, result);
        } else {
            return Continuation.ApplyDelegate(k, Bool.False);
        }
    }

    public static Thunk? syntax_p(Delegate k, List args) {
        if (args.Count() != 1) return Error(k, $"syntax?: expected one argument.");
        if (args.ElementAt(0) is Syntax) {
            return Continuation.ApplyDelegate(k, Bool.True);
        } else {
            return Continuation.ApplyDelegate(k, Bool.False);
        }
    }

    public static Thunk? pair_p(Delegate k, List args) {
        if (args.Count() != 1) return Error(k, $"pair?: expected one argument.");
        return Continuation.ApplyDelegate(k, args.ElementAt(0) is IPair ? Bool.True : Bool.False);
    }

    public static Thunk? datum_to_syntax(Delegate k, List args) {
        if (args.Count() != 2) return Error(k, $"datum->syntax: expected two arguments but got {args.Count()}.");
        if (args.ElementAt(0) is Syntax stx) {
            return Continuation.ApplyDelegate(k, Syntax.FromDatum(stx.SrcLoc, args.ElementAt(1)));
        } else {
            return Error(k, $"datum->syntax: expected first argument to be syntax, but got {args.ElementAt(0)}");
        }
    }

    public static Thunk? syntax_to_datum(Delegate k, List args) {
        if (args.Count() != 1) return Error(k, $"syntax->datum: expected one argument but got {args.Count()}.");
        if (args.ElementAt(0) is Syntax stx) {
            return Continuation.ApplyDelegate(k, Syntax.ToDatum(stx));
        } else {
            return Error(k, $"datum->syntax: expected first argument to be syntax, but got {args.ElementAt(0)}");
        }
    }

    public static Thunk? expand(Delegate k, List args) {
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

    public static Thunk? expand_once(Delegate k, List args) {
        if (args.Count() != 1) return Error(k, $"expand-once: expected a single argument but got {args.Count()}");
        if (args.ElementAt(0) is Syntax stx) {
            // TODO: what should expansion environment be?
            Syntax result = new MacroExpander().Expand(stx, ExpansionEnvironment.Default, once: true);
            return Continuation.ApplyDelegate(k, result);
        } else {
            stx = Syntax.FromDatum(null, args.ElementAt(0));
            Syntax result = new MacroExpander().Expand(stx, ExpansionEnvironment.Default, once: true);
            return Continuation.ApplyDelegate(k, result);
        }
    }

    internal static Thunk? Error(Delegate k, string msg, params Form[] rest) {
        // the reason we have to look up "error" in the global environment is that
        // in prelude we redefine error so that it uses the redefined call/cc that unwinds winders
        // otherwise *current-exception-handlers* isn't restored properly
        //
        // TODO: should we use raise or raise-continuable in builtins instead?
        // TODO: cache this search somehow
        Form errorExpr = Program.TopLevel[new Form.Symbol("error")];
        if (errorExpr is Procedure proc) {
            return proc.Apply(k, List.NewList(new String(msg)));
        } else {
            throw new Exception("error is not bound to a procedure");
        }
    }

    public static Thunk? error(Delegate k, List args) {
        // TODO: should probably be possible to use a different continuation than id
        // for example if we are reading in a whole script rather than an expr at REPL
        // TODO: print other args
        // TODO: stack trace
        Console.Error.Write("*** ERROR: ");
        Continuation.OneArgDelegate end = (x) => null;
        if (!args.Any()) {
            Console.Error.WriteLine("error: expected at least one argument");
            return Continuation.ApplyDelegate(end, Form.Void);
        }
        if (args.ElementAt(0) is not String msg)
        {
            Console.Error.WriteLine($"error: expected first argument to be a string message but got {args.ElementAt(0)}.");
        }
        else
        {
            Console.Error.WriteLine(msg);
        }
        return Continuation.ApplyDelegate(end, Form.Void);
    }

    public static Thunk? vector(Delegate k, List args) {
        return Continuation.ApplyDelegate(k, new Vector(args));

    }
    public static Thunk? make_record_type_descriptor(Delegate k, List args) {
        return Continuation.ApplyDelegate(k, new Record.TypeDescriptor(args));

    }

    public static Thunk? record_type_descriptor_p(Delegate k, List args) {
        if (args.Count() != 1) return Error(k, $"record-type-descriptor?: expected a single argument but got {args.Count()}");
        if (args.ElementAt(0) is Record.TypeDescriptor) {
            return Continuation.ApplyDelegate(k, Bool.True);
        }
        return Continuation.ApplyDelegate(k, Bool.False);

    }
    public static Thunk? record_constructor_descriptor_p(Delegate k, List args) {
        if (args.Count() != 1) return Error(k, $"record-constructor-descriptor?: expected a single argument but got {args.Count()}");
        if (args.ElementAt(0) is Record.ConstructorDescriptor) {
            return Continuation.ApplyDelegate(k, Bool.True);
        }
        return Continuation.ApplyDelegate(k, Bool.False);

    }

    public static Thunk? make_record_constructor_descriptor(Delegate k, List args) {
        return Continuation.ApplyDelegate(k, new Record.ConstructorDescriptor(args));
    }

    public static Thunk? vector_p(Delegate k, List args) {
        if (args.Count() != 1) return Error(k, $"vector?: expected a single argument but got {args.Count()}");
        var arg = args.ElementAt(0);
        if (arg is Record) {
            return Continuation.ApplyDelegate(k, Bool.False);
        }
        if (arg is Vector) {
            return Continuation.ApplyDelegate(k, Bool.True);
        }
        return Continuation.ApplyDelegate(k, Bool.False);

    }
    public static Thunk? record_p(Delegate k, List args) {
        if (args.Count() != 1) return Error(k, $"record?: expected a single argument but got {args.Count()}");
        var arg = args.ElementAt(0);
        if (arg is Record) {
            return Continuation.ApplyDelegate(k, Bool.True);
        }
        return Continuation.ApplyDelegate(k, Bool.False);

    }
    public static Thunk? record_predicate(Delegate k, List args) {
        if (args.Count() != 1) return Error(k, $"record-predicate: expected a single argument but got {args.Count()}");
        var arg = args.ElementAt(0);
        if (arg is not Record.TypeDescriptor rtd) {
            return Error(k, $"record-predicate: expected argument to be a record type descriptor but got {args.Count()}");
        }
        return Continuation.ApplyDelegate(k, rtd.Predicate());

    }

    public static Thunk? record_accessor(Delegate k, List args) {
        if (args.Count() != 2) return Error(k, $"record-accessor: expected two arguments but got {args.Count()}");
        if (args.ElementAt(0) is not Record.TypeDescriptor rtd) {
            return Error(k, $"record-predicate: expected argument to be a record type descriptor but got {args.ElementAt(0)}");
        }
        if (args.ElementAt(1) is not Integer i) {
            return Error(k, $"record-predicate: expected second argument to be an integer but got {args.ElementAt(1)}");
        }
        return Continuation.ApplyDelegate(k, rtd.Accessor(i));

    }
    public static Thunk? record_constructor(Delegate k, List args) {
        if (args.Count() != 1) return Error(k, $"record-constructor: expected a single argument but got {args.Count()}");
        var arg = args.ElementAt(0);
        if (arg is not Record.ConstructorDescriptor rcd) {
            return Error(k, $"record-constructor: expected argument to be a record constructor descriptor but got {args.Count()}");
        }
        return Continuation.ApplyDelegate(k, rcd.Constructor());

    }

    public static Thunk? vector_length(Delegate k, List args) {
        if (args.Count() != 1) return Error(k, $"vector-length: expected a single argument but got {args.Count()}");
        if (args.ElementAt(0) is Vector v) {
            return Continuation.ApplyDelegate(k, v.Length);
        } else {
            return Error(k, "vector-length: expected argument to be vector");

        }

    }

    public static Thunk? vector_ref(Delegate k, List args) {
        if (args.Count() != 2) return Error(k, $"vector-ref: expected two arguments but got {args.Count()}");
        if (args.ElementAt(0) is Vector v) {
            if (args.ElementAt(1) is Integer i) {
                if (v.TryGetAtIndex(i, out IForm? result)) {
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

    public static Thunk? eval(Delegate k, List args) {
        if (args is List.NonEmpty properList) {
            var datum = properList.Car;
            Console.WriteLine($"eval: first arg is {datum}");
            return Continuation.ApplyDelegate(k, Program.EvalNonCPS(datum));
        }
        else {
            return Error(k, $"eval: expected at least one argument");
        }
    }
}
