namespace Jig;

internal static partial class Builtins {

    internal static Thunk? match_syntax_macro(Delegate k, Form arg) {
        Syntax stx = arg as Syntax ?? throw new Exception($"match-syntax: expected syntax, got {arg.GetType()}");
        SyntaxList stxList = Syntax.E(stx) as SyntaxList ?? throw new Exception("match-syntax: syntax should expand to list");
        if (stxList.Count<Syntax>() < 2) throw new Exception(); // TODO: this should be a syntax error
        Syntax expr = stxList.ElementAt<Syntax>(1);
        Syntax.Identifier x = new (new Form.Symbol("x"));
        Syntax result = new Syntax(
            SyntaxList.FromParams(
                new Syntax(
                    SyntaxList.FromParams(
                        new Syntax.Identifier(new Form.Symbol("lambda")),
                        new Syntax(SyntaxList.FromParams(x)),
                        LambdaBodyForMatch(x, stxList.Skip<Syntax>(2))
                    )),
                expr
            )
        );
        return Continuation.ApplyDelegate(k, result);

    }

    private static Syntax LambdaBodyForMatch(Syntax x, IEnumerable<Syntax> clauses)
    {
        return new Syntax(MakeIfs(x, clauses));

    }


    private static SyntaxList MakeIfs (Syntax x, IEnumerable<Syntax> clauses) {
        // TODO: clean this up
        if (!clauses.Any()) throw new Exception();
        Syntax elseBranch =
            clauses.Count() == 1 ?
            new Syntax(SyntaxList.FromParams(
                new Syntax.Identifier(new Form.Symbol("error")),
                new Syntax.Literal(new String("match: couldn't find a match."))
            )) : 
            new Syntax(MakeIfs(x, clauses.Skip(1)));
          
        SyntaxList.NonEmpty thisClause =
            Syntax.E(clauses.ElementAt(0)) as SyntaxList.NonEmpty ?? throw new Exception(); // TODO: should be syntax error
        return (SyntaxList)SyntaxList.FromParams(
            new Syntax.Identifier(new Form.Symbol("if")),
            MakeConditionForMatchClause(x, thisClause.First),
            MakeThenForMatchClause(x, thisClause.First, thisClause.Rest),
            elseBranch
        );

    }

    private static IForm ParamsFromPattern(Syntax pattern) {
        switch (Syntax.E(pattern)) {
            case Number: return List.Null;
            case Bool: return List.Null;
            case List.Empty: return List.Null;
            case Form.Symbol: return pattern;
            case SyntaxList stxList:
                if (Form.IsKeyword("quote", pattern)) {
                    return List.Null;
                }
                if (Form.IsKeyword("quote-syntax", pattern)) {
                    return List.Null;
                }
                System.Collections.Generic.List<IForm> res = [];
                foreach (Syntax s in stxList.Cast<Syntax>()) {
                    res.Add(ParamsFromPattern(s));
                }
                return res.ToJigList();
            case IPair pair:
                return ParamsFromPattern(pair);
            default:
                throw new NotImplementedException($"ParamsFromPatterns: unhandled case {Syntax.E(pattern)}");
        }

    }
    private static IForm ParamsFromPattern(IForm form) {
        if (form is Syntax stx) {
            return ParamsFromPattern(stx);
        } else if (form is IPair pair) {
            return ParamsFromPattern(pair);
        } else {
            throw new NotImplementedException();
        }

    }

    private static IForm ParamsFromPattern(IPair pair) {
        return Pair.Cons(ParamsFromPattern(pair.Car), ParamsFromPattern(pair.Cdr));
    }

    private static Syntax ParamsForLambdaForMatchClauseThen(Syntax pattern) {
        var parameters = ParamsFromPattern(pattern);
        return new Syntax(Flatten(parameters));
    }

    private static Syntax MakeThenForMatchClause(Syntax x, Syntax pattern, List bodies) {
        // TODO: rewrite with cons and append
        SyntaxList result = 
            new System.Collections.Generic.List<Syntax> {
                new(new System.Collections.Generic.List<Syntax>
                {
                    new Syntax.Identifier(new Form.Symbol("lambda")),
                    ParamsForLambdaForMatchClauseThen(pattern)
                }.Concat<Syntax>(bodies.Cast<Syntax>()).ToSyntaxList())
            }.ToSyntaxList();
        SyntaxList args = Flatten(ArgsForLambdaForMatchClauseThen(x, pattern));
        result = result.Concat<Syntax>(args).ToSyntaxList();
        return new Syntax(result);
    }

    private static SyntaxList Flatten(IForm x) {

        switch (x) {
            case IEmptyList: return SyntaxList.Null;
            case Syntax stx: return SyntaxList.FromParams(stx);
            case IPair pair:
                return (SyntaxList)Flatten(pair.Car).Append(Flatten(pair.Cdr));
            default: throw new Exception($"Flatten: unhandled case {x} a {x.GetType()}");
        }

    }

    private static Syntax MakeCall(string functionName, params Syntax[] args) {
        var syntaxList = new System.Collections.Generic.List<Syntax>{
            new Syntax.Identifier(new Form.Symbol(functionName))
        }.Concat(args).ToSyntaxList();
        return new Syntax(syntaxList);
        


    }
    private static Syntax MakeConditionForMatchClause(Syntax x, Syntax pattern) {
        return Syntax.E(pattern) switch
        {
            Number => MakeCall("=", pattern, x),
            Bool => MakeCall("eqv?", pattern, x),
            Syntax => MakeCall("eqv?", pattern, x),
            IEmptyList => MakeCall("null?", x),
            Form.Symbol => new Syntax.Literal(Bool.True),
            SyntaxList syntaxList => MakeConditionForMatchClause(MakeCall("syntax-e", x), syntaxList),
            IPair pair => MakeConditionForMatchClause(MakeCall("syntax-e", x), pair.Car, pair.Cdr),
            _ => throw new NotImplementedException($"x = {x.Print()} and pattern = {pattern.Print()}"),
        };
    }

    private static Syntax MakeConditionForMatchClause(Syntax x, IForm car, IForm cdr)
    {
        Syntax carSyntax = car as Syntax ?? throw new Exception();
        if (cdr is Syntax cdrSyntax) {
            return MakeCall(
                "and",
                MakeCall("pair?", x),
                MakeConditionForMatchClause(MakeCall("car", x), carSyntax),
                MakeConditionForMatchClause(MakeCall("cdr", x), cdrSyntax));
        } else {
            if (cdr is IPair p) {
                return MakeCall(
                    "and",
                    MakeCall("pair?", x),
                        MakeConditionForMatchClause(MakeCall("car", x), carSyntax),
                        MakeConditionForMatchClause(MakeCall("cdr", x), p.Car, p.Cdr));
            } else {
                throw new NotImplementedException();
            }
        }
    }

    private static Syntax MakeConditionForMatchClause(Syntax x, SyntaxList pattern) {
        if (pattern.ElementAt<Syntax>(0) is Syntax.Identifier id && id.Symbol.Name == "quote") {
            return MakeCall("eqv?", x, new Syntax(pattern));
        }
        if (pattern.ElementAt<Syntax>(0) is Syntax.Identifier q && q.Symbol.Name == "quote-syntax") {
            return MakeCall("eqv?", x, new Syntax(pattern));
        }
        System.Collections.Generic.List<Syntax> firstPart = [
            new Syntax.Identifier(new Form.Symbol("and")),
            MakeCall("list?", x),
            MakeCall("=", MakeCall("length", x), new Syntax.Literal(new Integer(pattern.Count<Syntax>())))
        ];
        var secondPart = new System.Collections.Generic.List<Syntax>();
        for(int i = 0; i < pattern.Count<Syntax>(); i++) {
            secondPart.Add(MakeConditionForMatchClause(NthElementOfList(i, x), pattern.ElementAt<Syntax>(i)));
        }
        return new Syntax(firstPart.Concat<Syntax>(secondPart).ToSyntaxList());
    }

    private static Syntax NthElementOfList(int i, Syntax x) {
        return MakeCall("list-ref", x, new Syntax.Literal(new Integer(i)));
        // while (i > 0) {
        //     x = new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Form.Symbol("cdr")), x));
        //     i--;
        // }
        // return new Syntax(
        //     SyntaxList.FromParams(
        //         new Syntax.Identifier(new Form.Symbol("syntax-e")),
        //         new Syntax(
        //             SyntaxList.FromParams(
        //                 new Syntax.Identifier(new Form.Symbol("car")),
        //                 x))));
    }


    private static List ArgsForLambdaForMatchClauseThen(Syntax x, SyntaxList patterns) {
        if (patterns.ElementAt<Syntax>(0) is Syntax.Identifier id && id.Symbol.Name == "quote") {
            return SyntaxList.Null;
        }
        if (patterns.ElementAt<Syntax>(0) is Syntax.Identifier q && q.Symbol.Name == "quote-syntax") {
            return SyntaxList.Null;
        }
        System.Collections.Generic.List<IForm> result = [];
        for (int i = 0; i < patterns.Count<Syntax>(); i++){
            result.Add(ArgsForLambdaForMatchClauseThen(NthElementOfList(i, x), patterns.ElementAt<Syntax>(i)));
        }
        return result.ToJigList();

    }

    private static IForm ArgsForLambdaForMatchClauseThen(Syntax x, Syntax pattern)
    {
        return Syntax.E(pattern) switch
        {
            List.Empty => SyntaxList.Null,
            Number => SyntaxList.Null,
            Bool => SyntaxList.Null,
            Form.Symbol => x,
            SyntaxList stxList => ArgsForLambdaForMatchClauseThen(MakeCall("syntax-e", x), stxList),
            IPair pair => ArgsForLambdaForMatchClauseThen(MakeCall("syntax-e", x), pair),
            _ => throw new NotImplementedException(),
        };
    }
    private static IForm ArgsForLambdaForMatchClauseThen(Syntax x, IPair pair) {
        return Pair.Cons(
                ArgsForLambdaForMatchClauseThen(
                    x: MakeCall("car", x),
                    pattern: pair.Car),
                ArgsForLambdaForMatchClauseThen(
                    x: MakeCall("cdr", x),
                    pattern: pair.Cdr));
    }

    private static IForm ArgsForLambdaForMatchClauseThen(Syntax x, IForm pattern) {
        if (pattern is Syntax stx) {
            return ArgsForLambdaForMatchClauseThen(x, stx);
        } else if (pattern is IPair p) {
            return ArgsForLambdaForMatchClauseThen(x, p);
        } else {
            throw new NotImplementedException();
        }
    }
}