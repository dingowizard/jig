namespace Jig;

internal static partial class Builtins {

    internal static Thunk? match_syntax_macro(Delegate k, Form arg) {
        Syntax stx = arg as Syntax ?? throw new Exception($"match-syntax: expected syntax, got {arg.GetType()}");
        SyntaxList stxList = Syntax.E(stx) as SyntaxList ?? throw new Exception("match-syntax: syntax should expand to list");
        if (stxList.Count<Syntax>() < 2) throw new Exception(); // TODO: this should be a syntax error
        Syntax expr = stxList.ElementAt<Syntax>(1);
        Syntax.Identifier x = new (new Symbol("x"));
        Syntax result = new Syntax(
            SyntaxList.FromParams(
                new Syntax(
                    SyntaxList.FromParams(
                        new Syntax.Identifier(new Symbol("lambda")),
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


    private static SyntaxList MakeIfs (Syntax x, IEnumerable<Syntax> xs) {
        // TODO: clean this up
        var clauses = xs as Syntax[] ?? xs.ToArray();
        if (clauses.Length == 0) throw new Exception();
        var elseBranch =
            clauses.Length == 1 ?
            new Syntax(SyntaxList.FromParams(
                new Syntax.Identifier(new Symbol("error")),
                new Syntax.Literal(new String("match: couldn't find a match."))
            )) : 
            new Syntax(MakeIfs(x, clauses.Skip(1)));

        var thisClause = Syntax.E(clauses[0]) as SyntaxList.NonEmpty
                         ?? throw new Exception(); // TODO: should be syntax error
        return SyntaxList.FromParams(
            new Syntax.Identifier(new Symbol("if")),
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
            case Symbol: return pattern;
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
                    new Syntax.Identifier(new Symbol("lambda")),
                    ParamsForLambdaForMatchClauseThen(pattern)
                }.Concat(bodies.Cast<Syntax>()).ToSyntaxList())
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

    private static Syntax MakeCallFromName(string functionName, params Syntax[] args) {
        var syntaxList = new System.Collections.Generic.List<Syntax>{
            new Syntax.Identifier(new Symbol(functionName))
        }.Concat(args).ToSyntaxList();
        return new Syntax(syntaxList);
    }

    private static Syntax MakeConditionForMatchClause(Syntax x, Syntax pattern) {
        return Syntax.E(pattern) switch
        {
            Number => MakeCallFromName("=", pattern, x),
            Bool => MakeCallFromName("eqv?", pattern, x),
            Syntax => MakeCallFromName("eqv?", pattern, x),
            IEmptyList => MakeCallFromName("null?", x),
            Symbol => new Syntax.Literal(Bool.True),
            SyntaxList syntaxList => MakeConditionForMatchClause(MakeCallFromName("syntax-e", x), syntaxList),
            IPair pair => MakeConditionForMatchClause(MakeCallFromName("syntax-e", x), pair.Car, pair.Cdr),
            _ => throw new NotImplementedException($"x = {x.Print()} and pattern = {pattern.Print()}"),
        };
    }

    private static Syntax MakeConditionForMatchClause(Syntax x, IForm car, IForm cdr)
    {
        Syntax carSyntax = car as Syntax ?? throw new Exception();
        if (cdr is Syntax cdrSyntax) {
            return MakeCallFromName(
                "and",
                MakeCallFromName("pair?", x),
                MakeConditionForMatchClause(MakeCallFromName("car", x), carSyntax),
                MakeConditionForMatchClause(MakeCallFromName("cdr", x), cdrSyntax));
        } else {
            if (cdr is IPair p) {
                return MakeCallFromName(
                    "and",
                    MakeCallFromName("pair?", x),
                        MakeConditionForMatchClause(MakeCallFromName("car", x), carSyntax),
                        MakeConditionForMatchClause(MakeCallFromName("cdr", x), p.Car, p.Cdr));
            } else {
                throw new NotImplementedException();
            }
        }
    }

    private static Syntax MakeConditionForMatchClause(Syntax x, SyntaxList pattern) {
        if (pattern.ElementAt<Syntax>(0) is Syntax.Identifier { Symbol.Name: "quote" }) {
            return MakeCallFromName("eqv?", x, new Syntax(pattern));
        }
        if (pattern.ElementAt<Syntax>(0) is Syntax.Identifier { Symbol.Name: "quote-syntax" }) {
            return MakeCallFromName("eqv?", x, new Syntax(pattern));
        }
        System.Collections.Generic.List<Syntax> firstPart = [
            new Syntax.Identifier(new Symbol("and")),
            MakeCallFromName("list?", x),
            MakeCallFromName("=", MakeCallFromName("length", x), new Syntax.Literal(new Integer(pattern.Count<Syntax>())))
        ];
        var secondPart = new System.Collections.Generic.List<Syntax>();
        for(int i = 0; i < pattern.Count<Syntax>(); i++) {
            secondPart.Add(MakeConditionForMatchClause(NthElementOfList(i, x), pattern.ElementAt<Syntax>(i)));
        }
        return new Syntax(firstPart.Concat(secondPart).ToSyntaxList());
    }

    private static Syntax NthElementOfList(int i, Syntax x) {
        return MakeCallFromName("list-ref", x, new Syntax.Literal(new Integer(i)));
        // while (i > 0) {
        //     x = new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Symbol("cdr")), x));
        //     i--;
        // }
        // return new Syntax(
        //     SyntaxList.FromParams(
        //         new Syntax.Identifier(new Symbol("syntax-e")),
        //         new Syntax(
        //             SyntaxList.FromParams(
        //                 new Syntax.Identifier(new Symbol("car")),
        //                 x))));
    }


    private static List ArgsForLambdaForMatchClauseThen(Syntax x, SyntaxList patterns) {
        if (patterns.ElementAt<Syntax>(0) is Syntax.Identifier { Symbol.Name: "quote" }) {
            return SyntaxList.Null;
        }
        if (patterns.ElementAt<Syntax>(0) is Syntax.Identifier { Symbol.Name: "quote-syntax" }) {
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
            Symbol => x,
            SyntaxList stxList => ArgsForLambdaForMatchClauseThen(MakeCallFromName("syntax-e", x), stxList),
            IPair pair => ArgsForLambdaForMatchClauseThen(MakeCallFromName("syntax-e", x), pair),
            _ => throw new NotImplementedException(),
        };
    }
    private static IForm ArgsForLambdaForMatchClauseThen(Syntax x, IPair pair) {
        return Pair.Cons(
                ArgsForLambdaForMatchClauseThen(
                    x: MakeCallFromName("car", x),
                    pattern: pair.Car),
                ArgsForLambdaForMatchClauseThen(
                    x: MakeCallFromName("cdr", x),
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
