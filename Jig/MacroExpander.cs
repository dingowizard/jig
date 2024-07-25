using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Linq.Expressions;

namespace Jig;

public class MacroExpander {

    internal Scope TopLevelScope = new Scope();

    public MacroExpander() {

        Bindings = new Dictionary<Syntax.Identifier, Binding>();
    }

    internal Dictionary<Syntax.Identifier, Binding> Bindings {get;}

    IEnumerable<Syntax.Identifier> FindCandidateIdentifiers(Syntax.Identifier id) {
        IEnumerable<Syntax.Identifier> sameName = Bindings.Keys.Where(i => i.Symbol.Name == id.Symbol.Name);
        // if (id.Symbol.Name == "a") {
        //     Console.WriteLine($"The search id -- {id} -- has the following scope sets: {string.Join(',', id.ScopeSet)}");
        //     Console.WriteLine($"Found {sameName.Count()} bindings with same name.");
        //     foreach (var b in sameName) {
        //         Console.WriteLine($"\tscope sets: {string.Join(',', b.ScopeSet)}");
        //     }
        // }
        var result = sameName.Where(i => i.ScopeSet.IsSubsetOf(id.ScopeSet));
        return result;
    }

    internal bool TryResolve(Syntax.Identifier id, [NotNullWhen(returnValue: true)] out Binding? binding) {
        var candidates = FindCandidateIdentifiers(id);
        // if (id.Symbol.Name == "a") {
        //     Console.WriteLine($"TryResolve: found {candidates.Count()} candidates for 'a' at {id.SrcLoc} in {this.GetHashCode()}");
        // }
        if (!candidates.Any()) {
            binding = null;
            return false;
        }
        #pragma warning disable CS8600
        Syntax.Identifier maxID = candidates.MaxBy<Syntax.Identifier, int>(i => i.ScopeSet.Count);// ?? throw new Exception("impossible");
        Debug.Assert(maxID is not null);
#pragma warning restore CS8600
        CheckUnambiguous(maxID, candidates);
        binding = Bindings[maxID];
        return true;
    }

    static void CheckUnambiguous(Syntax.Identifier maxID, IEnumerable<Syntax.Identifier> candidates) {
        // TODO: understand this better
        foreach (var candidate in candidates) {
            if (!candidate.ScopeSet.IsSubsetOf(maxID.ScopeSet)) {
                throw new Exception($"ambiguous : {maxID}");
            }
        }

    }

    public  ParsedExpr Expand(Syntax stx, ExpansionEnvironment ee, bool definesAllowed = true, bool once = false) {
        if (ParsedVariable.TryParse(stx, this, out ParsedVariable? parsedVar)) {
            return parsedVar;
        }
        if (ParsedBegin.TryParse(stx, this, ee, definesAllowed, out ParsedBegin? parsedBegin)) {
            return parsedBegin;
        }
        if (ParsedIf.TryParse(stx, this, ee, out ParsedIf? ifForm)) {
            return ifForm;
        } else if (ParsedLambda.TryParse(stx, this, ee, out ParsedLambda? lambdaForm)) {
            return lambdaForm;
        } else if (ParsedSet.TryParse(stx, this, ee, out ParsedSet? setForm)) {
            return setForm;
        } else if (ParsedDefine.TryParse(stx, this, ee, out ParsedDefine? defineForm)) {
            if (definesAllowed) {
                return defineForm;
            } else {
                throw new Exception("syntax error: invalid context for definition.");
            }
        } else if (ParsedLiteral.TryParse(stx, out ParsedLiteral? literalExpr)) {
            return literalExpr;
        } else if (ParsedQuoteSyntax.TryParse(stx, out ParsedQuoteSyntax? quoteSyntaxExpr)) {
            return quoteSyntaxExpr;
        }
        if (Syntax.E(stx) is SyntaxList stxList) {
            if (Expr.IsKeyword("define-syntax", stx)) {
                return ExpandDefineSyntax(stx.SrcLoc, stxList, ee);
            } else {
                return ExpandApplication(stx, stxList, ee, once);
            }
        } else {
            throw new Exception($"Expand: unhandled syntax {stx}, a {stx.GetType()} @ {stx.SrcLoc}");
        }
    }


    private  ParsedExpr ExpandDefineSyntax(SrcLoc? srcLoc, SyntaxList stxList, ExpansionEnvironment ee)
    {
        Syntax.Identifier id = stxList.ElementAt<Syntax>(1) as Syntax.Identifier ?? throw new Exception() ;
        // Console.WriteLine($"define-syntax: {id}");
        id.Symbol.Binding = new Binding();
        Bindings.Add(id, id.Symbol.Binding);
        Syntax shouldBeLambdaExpr = stxList.ElementAt<Syntax>(2);
        if (!Expr.IsKeyword("lambda", shouldBeLambdaExpr)) {
            throw new Exception($"define-syntax: expected 2nd argument to be a transformer. Got {stxList.ElementAt<Syntax>(2)}");
        }

        // TODO: use ParsedLambda.TryParse? (for vars in set! and define as well?)
        ParsedLambda expandedLambdaExpr = (ParsedLambda)Expand(stxList.ElementAt<Syntax>(2), ee);
        Transformer transformer = EvaluateTransformer(expandedLambdaExpr);
        ee.AddTransformer(id.Symbol, transformer);
        Syntax result = new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Expr.Symbol("quote")), id), srcLoc);
        if (ParsedLiteral.TryParse(result, out ParsedLiteral? lit)) {
            return lit;
        } else {
            throw new Exception($"ExpandDefineSyntax: result should be a quoted symbol. Got {result}");
        }

    }

    private static Transformer EvaluateTransformer(ParsedLambda lambdaExprSyntax) {
        Procedure procedure = Program.EvalNonCPSNoExpand(lambdaExprSyntax) as Procedure ??
            throw new Exception($"define-syntax: second argument should evaluate to a transformer.");
        return new Transformer(procedure.Value as Func<Delegate, Expr, Thunk> ??
            throw new Exception($"define-syntax: second argument should be a transformer (got {procedure.Value})"));
    }


    private  ParsedExpr ExpandApplication(Syntax stx, SyntaxList stxList, ExpansionEnvironment ee, bool once = false)
    {
        if (stxList.ElementAt<Syntax>(0) is Syntax.Identifier id && ee.TryFindTransformer(id.Symbol, out Transformer? transformer)) {
            Scope macroExpansionScope = new Scope();
            Syntax.AddScope(stx, macroExpansionScope);
            // Console.WriteLine($"macro application: {stx}");
            Syntax output = transformer.Apply(stx);
            // var result = output;
            // Console.WriteLine($"toggling scope on {output}");
            Syntax.ToggleScope(output, macroExpansionScope);
            // Console.WriteLine($"{stx} => {output}");
            if (once) {
                if (ParsedQuoteSyntax.TryParse(new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Expr.Symbol("quote-syntax")), output)),
                                               out ParsedQuoteSyntax? quoteSyntax)) {
                    return quoteSyntax;
                } else {
                    throw new Exception($"ExpandApplication: couldn't make quote-syntax form for result");
                }
            } else {
                return Expand(output, ee);
            }
        } else {
            return ExpandList(stx.SrcLoc, stxList, ee);
        }
    }

    private  ParsedList ExpandList(SrcLoc? srcLoc, SyntaxList stxList, ExpansionEnvironment ee) {
        List<Syntax> xs = [];
        foreach (var x in stxList) {
            Syntax bodyExpr = Expand(x, ee, definesAllowed: false);
            xs.Add(bodyExpr);
        }
        return new ParsedList((SyntaxList)SyntaxList.FromIEnumerable(xs), srcLoc);
    }

    private  Syntax ExpandSet(SrcLoc? srcLoc, SyntaxList stxList, ExpansionEnvironment ee)
    {
        List<Syntax> xs = new List<Syntax>();
        Debug.Assert(stxList.Count<Syntax>() == 3);
        xs.Add(stxList.ElementAt<Syntax>(0)); // set!
        foreach (var x in stxList.Skip<Syntax>(1)) {
            Syntax bodyExpr = Expand(x, ee);
            xs.Add(bodyExpr);
        }
        return new Syntax(SyntaxList.FromIEnumerable(xs), srcLoc);
    }

    private  Syntax ExpandDefine(SrcLoc? srcLoc, SyntaxList stxList, ExpansionEnvironment ee)
    {
        List<Syntax> xs = new List<Syntax>();
        Debug.Assert(stxList.Count<Syntax>() == 3); // TODO: this should be a syntax error
        xs.Add(stxList.ElementAt<Syntax>(0));
        Syntax.Identifier id = stxList.ElementAt<Syntax>(1) as Syntax.Identifier
            ?? throw new Exception($"ExpandDefine: expected first argument to be identifier. Got {stxList.ElementAt<Syntax>(1)}");
        // var newScope = new Scope();
        // Syntax.AddScope(id, newScope);
        if (!Bindings.ContainsKey(id)) {
            id.Symbol.Binding = new Binding();
            Bindings.Add(id, id.Symbol.Binding);
        }
        xs.Add(id);
        var x = stxList.ElementAt<Syntax>(2);
        xs.Add(Expand(x, ee));
        return new Syntax(SyntaxList.FromIEnumerable(xs), srcLoc);
    }


    private  Syntax ExpandLambda(SrcLoc? srcLoc, SyntaxList stxList, ExpansionEnvironment ee) {
        List<Syntax> xs = new List<Syntax>();
        xs.Add(stxList.ElementAt<Syntax>(0)); // lamdbda keyword
        var newScope = new Scope();
        var parameters = stxList.ElementAt<Syntax>(1);
        Syntax.AddScope(parameters, newScope); // TODO: is this necessary? Seems so. deleting it breaks a lot of tests.
        // create a new binding for each parameter
        if (Syntax.E(parameters) is SyntaxList psStxList) {
            foreach(var stx in psStxList) {
                Syntax.Identifier id = stx as Syntax.Identifier ?? throw new Exception($"ExpandLambda: expected parameters to be identifiers, but got {stx}");
                Binding binding = new Binding();
                id.Symbol.Binding = binding;
                Bindings.Add(id, binding);
            }
        } else if (Syntax.E(parameters) is IPair pair) {
            while (pair.Cdr is IPair cdrPair) {
                if (pair.Car is Syntax.Identifier ident) {
                    Binding binding = new Binding();
                    ident.Symbol.Binding = binding;
                    Bindings.Add(ident, binding);
                }
                pair = cdrPair;
            }
            if (pair.Cdr is Syntax.Identifier id) {
                Binding binding = new Binding();
                id.Symbol.Binding = binding;
                Bindings.Add(id, binding);
            }
        } else if (parameters is Syntax.Identifier psId) {
                Binding binding = new Binding();
                psId.Symbol.Binding = binding;
                Bindings.Add(psId, binding);
        } else if (Syntax.E(parameters) is List.NullType) {
            
        } else {
            throw new Exception($"ExpandLambda: expected parameters to be list or identifier, got {Syntax.E(parameters)}");
        }
        xs.Add(parameters);
        foreach (var x in stxList.Skip<Syntax>(2)) {
            Syntax.AddScope(x, newScope);
            Syntax bodyExpr = Expand(x, ee);
            xs.Add(bodyExpr);
        }
        return new Syntax(SyntaxList.FromIEnumerable(xs), srcLoc);
    }

}

public class ExpansionEnvironment {

    public ExpansionEnvironment(Dictionary<Expr.Symbol, Transformer> dict) {
        _dict = dict;
    }

    private static Thunk? match_macro(Delegate k, Expr arg) {
        // TODO: we need to make a local variable to hold the result of evaluating the expression to match
        // but how to do this hygienically?
        // is it sufficient just to create a binding form and variable?
        Syntax stx = arg as Syntax ?? throw new Exception($"match: expected syntax, got {arg.GetType()}");
        SyntaxList stxList = Syntax.E(stx) as SyntaxList ?? throw new Exception("match: syntax should expand to list");
        if (stxList.Count<Syntax>() < 2) throw new Exception(); // TODO: this should be a syntax error
        Syntax expr = stxList.ElementAt<Syntax>(1);
        Syntax.Identifier x = new (new Expr.Symbol("x"));
        Syntax result = new Syntax(
            SyntaxList.FromParams(
                new Syntax(
                    SyntaxList.FromParams(
                        new Syntax.Identifier(new Expr.Symbol("lambda")),
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
        Debug.Assert(clauses.Count() > 0);
        Syntax elseBranch =
            clauses.Count() == 1 ?
            new Syntax(SyntaxList.FromParams(
                new Syntax.Identifier(new Expr.Symbol("error")),
                new Syntax.Literal(new Expr.String("match: couldn't find a match."))
            )) : 
            new Syntax(MakeIfs(x, clauses.Skip(1)));
          
        SyntaxList thisClause = Syntax.E(clauses.ElementAt(0)) as SyntaxList ?? throw new Exception();
        return (SyntaxList)SyntaxList.FromParams(
            new Syntax.Identifier(new Expr.Symbol("if")),
            MakeConditionForMatchClause(x, thisClause.First),
            MakeThenForMatchClause(x, thisClause.First, thisClause.Rest),
            elseBranch
        );

    }

    private static Syntax MakeThenForMatchClause(Syntax x, Syntax pattern, List bodies) {
        // TODO: UGH!!
        SyntaxList result = 
            (SyntaxList)SyntaxList.FromIEnumerable(
            new List<Syntax> {
                new Syntax(
                    SyntaxList.FromIEnumerable(
                        new List<Syntax> {
                        new Syntax.Identifier(new Expr.Symbol("lambda")),
                        ParamsForLambdaForMatchClauseThen(pattern)}.
                        Concat<Syntax>(bodies.Cast<Syntax>())
                    ))
            });
        List args = Flatten(ArgsForLambdaForMatchClauseThen(x, pattern));
        if (args is SyntaxList properArgs) {
            result = (SyntaxList)SyntaxList.FromIEnumerable(result.Concat<Syntax>(properArgs));
        }
        return new Syntax(result);
    }


    private static List FlattenPair(Expr car, Syntax cdr) {
        // if this gets called, cdr is not a list
        switch (car) {
            case List.NullType: return Flatten(cdr);
            case IPair pair: return (List)Flatten(pair.Car).Append(Flatten(cdr));
            case Syntax stxCar: return (List)Expr.Pair.Cons(stxCar, Flatten(cdr));
            default: throw new Exception();

        }

    }
    private static List FlattenPair(Expr first, List rest) {
        switch (first) {
            case List.NullType: return Flatten(rest);
            case IPair pair: return (List)Flatten(pair.Car).Append(Flatten(rest));
            case Syntax stxCar: return (List)Expr.Pair.Cons(stxCar, Flatten(rest));
            default: throw new Exception();
        }

    }
    private static List FlattenPair(Expr first, SyntaxPair rest) {
        switch (first) {
            case List.NullType: return Flatten(rest);
            case IPair pair: return (List)Flatten(pair.Car).Append(Flatten(rest));
            case Syntax stxCar: return (List)Expr.Pair.Cons(stxCar, Flatten(rest));
            default: throw new Exception();
        }

    }

    private static List Flatten(Expr x) {

        switch (x) {
            case List.NullType: return List.Empty;
            case Syntax stx: return SyntaxList.FromParams(stx);
            case List.NonEmpty list: return FlattenPair(list.Car, list.Rest);
            case IPair pair:
                if (pair.Cdr is Syntax stxCdr) {
                    return FlattenPair(pair.Car, stxCdr);
                } else if (pair.Cdr is List l) {
                    return FlattenPair(pair.Car, l);
                } else if (pair.Cdr is SyntaxPair stxPairCdr) {
                    // TODO: sigh
                    return FlattenPair(pair.Car, stxPairCdr);
                } else {
                    throw new Exception($"In ExpansionEnvironment.Flatten: pair.Cdr is {pair.Cdr.Print()}, a {pair.Cdr.GetType()}");
                }
            default: throw new Exception();
        }

    }

    private static List ArgsForLambdaForMatchClauseThen(Syntax x, SyntaxList patterns) {
        List<Expr> result = [];
        for (int i = 0; i < patterns.Count<Syntax>(); i++){
            result.Add(ArgsForLambdaForMatchClauseThen(NthElementOfList(i, x), patterns.ElementAt<Syntax>(i)));
        }
        return List.NonEmpty.ListFromEnumerable(result);

    }

    private static Expr ArgsForLambdaForMatchClauseThen(Syntax x, Syntax pattern)
    {
        return Syntax.E(pattern) switch
        {
            List.NullType => SyntaxList.FromParams(),
            Expr.Number => SyntaxList.FromParams(),
            Expr.Symbol => x,
            SyntaxList stxList => ArgsForLambdaForMatchClauseThen(x, stxList),
            Expr.Pair pair => (Expr)Expr.Pair.Cons(
                ArgsForLambdaForMatchClauseThen(
                    x: new Syntax(SyntaxList.FromParams(
                        new Syntax.Identifier(new Expr.Symbol("car")),
                        x)),
                    pattern: (Syntax) pair.Car),
                ArgsForLambdaForMatchClauseThen(
                    x: new Syntax(SyntaxList.FromParams(
                        new Syntax.Identifier(new Expr.Symbol("cdr")),
                        x)),
                    pattern: (Syntax)pair.Cdr)),
            _ => throw new NotImplementedException(),
        };
    }

    private static Expr ParamsFromPattern(Syntax pattern) {
        switch (Syntax.E(pattern)) {
            case Expr.Number: return List.Empty;
            case List.NullType: return List.Empty;
            case Expr.Symbol: return pattern;
            case SyntaxList stxList:
                List<Expr> res = [];
                foreach (var s in stxList) {
                    res.Add(ParamsFromPattern(s));
                }
                return List.ListFromEnumerable(res);
            case Expr.Pair pair:
                if (pair.Car is Syntax x && pair.Cdr is Syntax y ) {
                    return (Expr) Expr.Pair.Cons(ParamsFromPattern(x),
                                                 ParamsFromPattern(y));
                }
                throw new NotImplementedException();
            default:
                throw new NotImplementedException();
        }

    }

    private static Syntax ParamsForLambdaForMatchClauseThen(Syntax pattern) {
        return new Syntax(Flatten(ParamsFromPattern(pattern)));
    }

    private static Syntax MakeConditionForMatchClause(Syntax x, SyntaxList pattern) {
        List<Syntax> firstPart = [
            new Syntax.Identifier(new Expr.Symbol("and")),
            new Syntax(SyntaxList.FromParams(
                new Syntax.Identifier(new Expr.Symbol("list?")),
                x
            )),
            new Syntax(SyntaxList.FromParams(
                new Syntax.Identifier(new Expr.Symbol("=")),
                    new Syntax(SyntaxList.FromParams(
                        new Syntax.Identifier(new Expr.Symbol("length")),
                        x
                    )),
                    new Syntax.Literal(new Expr.IntegerNumber(pattern.Count<Syntax>()))))
        ];
        var secondPart = new List<Syntax>();
        for(int i = 0; i < pattern.Count<Syntax>(); i++) {
            secondPart.Add(MakeConditionForMatchClause(NthElementOfList(i, x), pattern.ElementAt<Syntax>(i)));
        }
        return new Syntax(SyntaxList.FromIEnumerable(firstPart.Concat<Syntax>(secondPart)));
    }

    private static Syntax NthElementOfList(int i, Syntax x) {
        while (i > 0) {
            x = new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Expr.Symbol("cdr")), x));
            i--;
        }
        return new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Expr.Symbol("car")), x));
    }

    private static Syntax MakeConditionForMatchClause(Syntax x, Expr car, Expr cdr) {
        // TODO: Pair<Syntax> type?
        Syntax carSyntax = car as Syntax ?? throw new Exception();
        Syntax cdrSyntax = cdr as Syntax ?? throw new Exception();
        return new Syntax(
            SyntaxList.FromParams(
                new Syntax.Identifier(new Expr.Symbol("and")),
                new Syntax(SyntaxList.FromParams(
                    new Syntax.Identifier(new Expr.Symbol("pair?")),
                    x
                )),
                MakeConditionForMatchClause(x, carSyntax),
                MakeConditionForMatchClause(x, cdrSyntax))
        );
    }

    private static Syntax MakeNullTest(Syntax x) {
        return new Syntax(
            SyntaxList.FromParams(
                new Syntax.Identifier(new Expr.Symbol("null?")),
                x
        ));
    }

    private static Syntax MakeConditionForMatchClause(Syntax x, Syntax pattern) {
        return Syntax.E(pattern) switch
        {
            Expr.Number n => MakeNumEqTest(pattern, x),
            List.NullType => MakeNullTest(x),
            Expr.Symbol => new Syntax.Literal(Expr.Bool.True),
            SyntaxList syntaxList => MakeConditionForMatchClause(x, syntaxList),
            IPair pair => MakeConditionForMatchClause(x, pair.Car, pair.Cdr),
            _ => throw new NotImplementedException($"x = {x.Print()} and pattern = {pattern.Print()}"),
        };
    }

    private static Syntax MakeNumEqTest(Syntax n, Syntax x)
    {
        return new Syntax(
            SyntaxList.FromParams(
                new Syntax.Identifier(new Expr.Symbol("=")),
                x,
                n
        ));
    }

    private static Thunk? and_macro(Delegate k, Expr x) {
        Syntax stx = x as Syntax ?? throw new Exception($"and: expected syntax, got {x.GetType()}");
        Syntax result;
        SyntaxList stxList = Syntax.E(stx) as SyntaxList ?? throw new Exception("and: syntax should expand to list");
        if (stxList.Count<Syntax>() == 1) { // E.g. (and)
            result = new Syntax.Literal(Expr.Bool.True, null);
            return Continuation.ApplyDelegate(k, result);
        }
        if (stxList.Count<Syntax>() == 2) { // Eg (and 1)
            result = stxList.ElementAt<Syntax>(1);
            return Continuation.ApplyDelegate(k, result);
        }
        Syntax first = stxList.ElementAt<Syntax>(1);
        result = new Syntax(
            SyntaxList.FromParams(new Syntax.Identifier(new Expr.Symbol("if")),
                                    first,
                                    new Syntax(
                                    SyntaxList.FromIEnumerable(new List<Syntax>{
                                        new Syntax.Identifier(new Expr.Symbol("and"))
                                                                            }.Concat<Syntax>(stxList.Skip<Syntax>(2))),

                                    new SrcLoc()),
                                    new Syntax.Literal(Expr.Bool.False)),
            stx.SrcLoc);
        return Continuation.ApplyDelegate(k, result);

    }

    private static Thunk? quasiquote_macro(Delegate k, Expr x) {
        Syntax stx = x as Syntax ?? throw new Exception($"quasiquote: expected syntax, got {x.GetType()}");
        SyntaxList stxList = Syntax.E(stx) as SyntaxList ?? throw new Exception("quasiquote: syntax should expand to list");
        Syntax result;
        if (stxList.Count<Syntax>() != 2) { // Eg (quasiquote x)
            throw new Exception($"quasiquote: expected exactly one argument");
        }
        Syntax arg = stxList.ElementAt<Syntax>(1);
        Expr argE = Syntax.E(arg);
        if (argE is List.NullType ||
            argE is not IPair) {
            result = new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Expr.Symbol("quote")),
                                                      arg),
                                stx.SrcLoc);
        } else if (argE is SyntaxList stxListArg) {
            if (Expr.IsKeyword("unquote", arg)) { // (quasiquote (unquote x))
                if (stxListArg.Count<Syntax>() != 2) {
                    throw new Exception($"unquote: expected exactly one argument. Got {stxListArg.Count<Syntax>() - 1}");
                } else {
                    result = stxListArg.ElementAt<Syntax>(1);
                }
            } else if (Syntax.E(stxListArg.ElementAt<Syntax>(0)) is not SyntaxList) { // (quasiquote (x . rest)) where x is not a pair
                                                                       // => (cons (quote x) (quasiquote rest))
                result = new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Expr.Symbol("cons"), null),
                                                          new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Expr.Symbol("quote")),
                                                                                           stxListArg.ElementAt<Syntax>(0)),
                                                                     new SrcLoc()),
                                                          new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Expr.Symbol("quasiquote")),
                                                                                           new Syntax(SyntaxList.FromIEnumerable(stxListArg.Skip<Syntax>(1)))),
                                                                     new SrcLoc())),
                                    stx.SrcLoc);

            } else if (Syntax.E(stxListArg.ElementAt<Syntax>(0)) is SyntaxList slist) { // (quasiquote ((car . cdr) . rest))
                if (Expr.IsKeyword("unquote-splicing", stxListArg.ElementAt<Syntax>(0))) {
                    // (append (car (cdr slist) (quasiquote rest))
                    if (slist.Count<Syntax>() != 2) {
                        throw new Exception("unquote-splicing: expected exactly one argument.");
                    }
                    Syntax listToSpliceStx = slist.ElementAt<Syntax>(1);
                    // if (Syntax.E(listToSpliceStx) is not SyntaxList) {
                    //     throw new Exception("unquote-splicing: expected a list argument");
                    // }
                    result = new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Expr.Symbol("append")),
                                                              listToSpliceStx,
                                                              new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Expr.Symbol("quasiquote")),
                                                                                           new Syntax(SyntaxList.FromIEnumerable(stxListArg.Skip<Syntax>(1)))),
                                                                     new SrcLoc())),
                                    stx.SrcLoc);

                } else {
                    // (cons (quasiquote slist) (quasiquote rest))
                    result = new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Expr.Symbol("cons")),
                                                          new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Expr.Symbol("quasiquote")),
                                                                                           stxListArg.ElementAt<Syntax>(0)),
                                                                     new SrcLoc()),
                                                          new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Expr.Symbol("quasiquote")),
                                                                                           new Syntax(SyntaxList.FromIEnumerable(stxListArg.Skip<Syntax>(1)))),
                                                                     new SrcLoc())),
                                    stx.SrcLoc);

                }

            } else {
                throw new Exception($"malformed quasiquote: {stx}");
            }

        } else {
            throw new Exception($"malformed quasiquote: {stx}");
        }
        return Continuation.ApplyDelegate(k, result);

    }


    public static ExpansionEnvironment Default {get;} =
        new ExpansionEnvironment(new Dictionary<Expr.Symbol, Transformer>{
            {new Expr.Symbol("and"), new Transformer((Func<Delegate, Expr, Thunk?>) and_macro)},
            {new Expr.Symbol("match"), new Transformer((Func<Delegate, Expr, Thunk?>) match_macro)},
            {new Expr.Symbol("quasiquote"), new Transformer((Func<Delegate, Expr, Thunk?>) quasiquote_macro)},
            }
        );

    public bool TryFindTransformer(Expr.Symbol sym, [NotNullWhen(returnValue: true)] out Transformer? macro) {
        if (_dict.TryGetValue(sym, out Transformer? result)) {
            macro = result;
            return true;
        }
        macro = null;
        return false;
    }

    public void AddTransformer(Expr.Symbol sym, Transformer transformer) {
        _dict[sym] = transformer;
    }

    private Dictionary<Expr.Symbol, Transformer> _dict;

}

public class Binding {
    static int count = 0;

    public Binding() {
        count++;
    }
    //TODO: why can't scope be like this? (scope needs a member to work. maybe because it has to define gethashcode and equals?)
    //TODO: should the binding contain the scope that it comes from?
    //TODO: can Scope and binding classes be combined in some way?
    public static Binding TopLevel {get;} = new Binding();
    public override string ToString() => $"binding{count}";
}
