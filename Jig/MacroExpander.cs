using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using Microsoft.Scripting.Utils;

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
            if (Form.IsKeyword("define-syntax", stx)) {
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
        // TODO: use ParsedLambda.TryParse? (for vars in set! and define as well?)
        ParsedLambda expandedLambdaExpr = Expand(stxList.ElementAt<Syntax>(2), ee) as ParsedLambda ?? throw new Exception($"define-syntax: expected 2nd argument to be a transformer. Got {stxList.ElementAt<Syntax>(2)}");
        Transformer transformer = EvaluateTransformer(expandedLambdaExpr); // TODO: should this part actually happen at runtime? then eval would need expansion env
        ee.AddTransformer(id.Symbol, transformer);
        Syntax result = new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Form.Symbol("quote")), id), srcLoc);
        if (ParsedLiteral.TryParse(result, out ParsedLiteral? lit)) {
            return lit;
        } else {
            throw new Exception($"ExpandDefineSyntax: result should be a quoted symbol. Got {result}");
        }

    }

    private static Transformer EvaluateTransformer(ParsedLambda lambdaExprSyntax) {
        Procedure procedure = Program.EvalNonCPSNoExpand(lambdaExprSyntax) as Procedure ??
            throw new Exception($"define-syntax: second argument should evaluate to a transformer.");
        return new Transformer(procedure.Value as Func<Delegate, Form, Thunk> ??
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
            // Console.WriteLine($"\t => {output}");
            if (once) {
                if (ParsedQuoteSyntax.TryParse(new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Form.Symbol("quote-syntax")), output)),
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
        System.Collections.Generic.List<ParsedExpr> xs = [];
        foreach (Syntax x in stxList.Cast<Syntax>()) {
            ParsedExpr bodyExpr = Expand(x, ee, definesAllowed: false);
            xs.Add(bodyExpr);
        }
        return new ParsedList(xs, srcLoc);
    }

    private  Syntax ExpandSet(SrcLoc? srcLoc, SyntaxList stxList, ExpansionEnvironment ee)
    {
        System.Collections.Generic.List<Syntax> xs = [];
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
        System.Collections.Generic.List<Syntax> xs = [];
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
        System.Collections.Generic.List<Syntax> xs = [];
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
        } else if (Syntax.E(parameters) is List.Empty) {
            
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

    public ExpansionEnvironment(Dictionary<Form.Symbol, Transformer> dict) {
        _dict = dict;
    }

    private static Thunk? match_macro(Delegate k, Form arg) {
        Syntax stx = arg as Syntax ?? throw new Exception($"match: expected syntax, got {arg.GetType()}");
        SyntaxList stxList = Syntax.E(stx) as SyntaxList ?? throw new Exception("match: syntax should expand to list");
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
        Debug.Assert(clauses.Count() > 0);
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
            SyntaxList stxList => ArgsForLambdaForMatchClauseThen(x, stxList),
            IPair pair => ArgsForLambdaForMatchClauseThen(x, pair),
            _ => throw new NotImplementedException(),
        };
    }
    private static IForm ArgsForLambdaForMatchClauseThen(Syntax x, IPair pair) {
        return Pair.Cons(
                ArgsForLambdaForMatchClauseThen(
                    x: new Syntax(SyntaxList.FromParams(
                        new Syntax.Identifier(new Form.Symbol("car")),
                        x)),
                    pattern: pair.Car),
                ArgsForLambdaForMatchClauseThen(
                    x: new Syntax(SyntaxList.FromParams(
                        new Syntax.Identifier(new Form.Symbol("cdr")),
                        x)),
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

    private static Syntax MakeConditionForMatchClause(Syntax x, SyntaxList pattern) {
        if (pattern.ElementAt<Syntax>(0) is Syntax.Identifier id && id.Symbol.Name == "quote") {
            return new Syntax(
                SyntaxList.FromParams(
                    new Syntax.Identifier(new Form.Symbol("eqv?")),
                    x,
                    new Syntax(pattern)));
        }
        if (pattern.ElementAt<Syntax>(0) is Syntax.Identifier q && q.Symbol.Name == "quote-syntax") {
            return new Syntax(
                SyntaxList.FromParams(
                    new Syntax.Identifier(new Form.Symbol("eqv?")),
                    x,
                    new Syntax(pattern)));
        }
        System.Collections.Generic.List<Syntax> firstPart = [
            new Syntax.Identifier(new Form.Symbol("and")),
            new Syntax(SyntaxList.FromParams(
                new Syntax.Identifier(new Form.Symbol("list?")),
                x
            )),
            new Syntax(SyntaxList.FromParams(
                new Syntax.Identifier(new Form.Symbol("=")),
                    new Syntax(SyntaxList.FromParams(
                        new Syntax.Identifier(new Form.Symbol("length")),
                        x
                    )),
                    new Syntax.Literal(new Integer(pattern.Count<Syntax>()))))
        ];
        var secondPart = new System.Collections.Generic.List<Syntax>();
        for(int i = 0; i < pattern.Count<Syntax>(); i++) {
            secondPart.Add(MakeConditionForMatchClause(NthElementOfList(i, x), pattern.ElementAt<Syntax>(i)));
        }
        return new Syntax(SyntaxList.FromIEnumerable(firstPart.Concat<Syntax>(secondPart)));
    }

    private static Syntax NthElementOfList(int i, Syntax x) {
        while (i > 0) {
            x = new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Form.Symbol("cdr")), x));
            i--;
        }
        return new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Form.Symbol("car")), x));
    }

    private static Syntax MakeConditionForMatchClause(Syntax x, IForm car, IForm cdr) {
        // TODO: Pair<Syntax> type?
        Syntax carSyntax = car as Syntax ?? throw new Exception();
        if (cdr is Syntax cdrSyntax) {
            return new Syntax(
                SyntaxList.FromParams(
                    new Syntax.Identifier(new Form.Symbol("and")),
                    new Syntax(SyntaxList.FromParams(
                        new Syntax.Identifier(new Form.Symbol("pair?")),
                        x
                    )),
                    MakeConditionForMatchClause(new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Form.Symbol("car")), x)), carSyntax),
                    MakeConditionForMatchClause(new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Form.Symbol("cdr")), x)), cdrSyntax))
            );
        } else {
            if (cdr is IPair p) {
                return new Syntax(
                    SyntaxList.FromParams(
                        new Syntax.Identifier(new Form.Symbol("and")),
                        new Syntax(SyntaxList.FromParams(
                            new Syntax.Identifier(new Form.Symbol("pair?")),
                            x
                        )),
                        MakeConditionForMatchClause(new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Form.Symbol("car")), x)), carSyntax),
                        MakeConditionForMatchClause(
                            new Syntax(
                                SyntaxList.FromParams(new Syntax.Identifier(new Form.Symbol("cdr")), x)),
                                p.Car,
                                p.Cdr))
                );
            } else {
                throw new NotImplementedException();
            }
        }
    }

    private static Syntax MakeNullTest(Syntax x) {
        return new Syntax(
            SyntaxList.FromParams(
                new Syntax.Identifier(new Form.Symbol("null?")),
                x
        ));
    }

    private static Syntax MakeConditionForMatchClause(Syntax x, Syntax pattern) {
        return Syntax.E(pattern) switch
        {
            Number n => MakeNumEqTest(pattern, x),
            Bool b => new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Form.Symbol("eqv?")), pattern, x)),
            Syntax => new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Form.Symbol("eqv?")), pattern, x)),
            List.Empty => MakeNullTest(x),
            Form.Symbol => new Syntax.Literal(Bool.True),
            SyntaxList syntaxList => MakeConditionForMatchClause(x, syntaxList),
            IPair pair => MakeConditionForMatchClause(x, pair.Car, pair.Cdr),
            _ => throw new NotImplementedException($"x = {x.Print()} and pattern = {pattern.Print()}"),
        };
    }

    private static Syntax MakeNumEqTest(Syntax n, Syntax x)
    {
        return new Syntax(
            SyntaxList.FromParams(
                new Syntax.Identifier(new Form.Symbol("=")),
                x,
                n
        ));
    }

    private static Thunk? and_macro(Delegate k, Form x) {
        Syntax stx = x as Syntax ?? throw new Exception($"and: expected syntax, got {x.GetType()}");
        Syntax result;
        SyntaxList stxList = Syntax.E(stx) as SyntaxList ?? throw new Exception("and: syntax should expand to list");
        if (stxList.Count<Syntax>() == 1) { // E.g. (and)
            result = new Syntax.Literal(Bool.True, null);
            return Continuation.ApplyDelegate(k, result);
        }
        if (stxList.Count<Syntax>() == 2) { // Eg (and 1)
            result = stxList.ElementAt<Syntax>(1);
            return Continuation.ApplyDelegate(k, result);
        }
        Syntax first = stxList.ElementAt<Syntax>(1);
        result = new Syntax(
            SyntaxList.FromParams(new Syntax.Identifier(new Form.Symbol("if")),
                                    first,
                                    new Syntax(
                                    SyntaxList.FromIEnumerable(new System.Collections.Generic.List<Syntax>{
                                        new Syntax.Identifier(new Form.Symbol("and"))
                                                                            }.Concat<Syntax>(stxList.Skip<Syntax>(2))),

                                    new SrcLoc()),
                                    new Syntax.Literal(Bool.False)),
            stx.SrcLoc);
        return Continuation.ApplyDelegate(k, result);

    }

    private static Thunk? quasiquote_macro(Delegate k, Form x) {
        Syntax stx = x as Syntax ?? throw new Exception($"quasiquote: expected syntax, got {x.GetType()}");
        SyntaxList stxList = Syntax.E(stx) as SyntaxList ?? throw new Exception("quasiquote: syntax should expand to list");
        Syntax result;
        if (stxList.Count<Syntax>() != 2) { // Eg (quasiquote x)
            throw new Exception($"quasiquote: expected exactly one argument");
        }
        Syntax arg = stxList.ElementAt<Syntax>(1);
        IForm argE = Syntax.E(arg);
        if (argE is List.Empty ||
            argE is not IPair) {
            result = new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Form.Symbol("quote")),
                                                      arg),
                                stx.SrcLoc);
        } else if (argE is SyntaxList stxListArg) {
            if (Form.IsKeyword("unquote", arg)) { // (quasiquote (unquote x))
                if (stxListArg.Count<Syntax>() != 2) {
                    throw new Exception($"unquote: expected exactly one argument. Got {stxListArg.Count<Syntax>() - 1}");
                } else {
                    result = stxListArg.ElementAt<Syntax>(1);
                }
            } else if (Syntax.E(stxListArg.ElementAt<Syntax>(0)) is not SyntaxList) { // (quasiquote (x . rest)) where x is not a pair
                                                                       // => (cons (quote x) (quasiquote rest))
                result = new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Form.Symbol("cons"), null),
                                                          new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Form.Symbol("quote")),
                                                                                           stxListArg.ElementAt<Syntax>(0)),
                                                                     new SrcLoc()),
                                                          new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Form.Symbol("quasiquote")),
                                                                                           new Syntax(SyntaxList.FromIEnumerable(stxListArg.Skip<Syntax>(1)))),
                                                                     new SrcLoc())),
                                    stx.SrcLoc);

            } else if (Syntax.E(stxListArg.ElementAt<Syntax>(0)) is SyntaxList slist) { // (quasiquote ((car . cdr) . rest))
                if (Form.IsKeyword("unquote-splicing", stxListArg.ElementAt<Syntax>(0))) {
                    // (append (car (cdr slist) (quasiquote rest))
                    if (slist.Count<Syntax>() != 2) {
                        throw new Exception("unquote-splicing: expected exactly one argument.");
                    }
                    Syntax listToSpliceStx = slist.ElementAt<Syntax>(1);
                    // if (Syntax.E(listToSpliceStx) is not SyntaxList) {
                    //     throw new Exception("unquote-splicing: expected a list argument");
                    // }
                    result = new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Form.Symbol("append")),
                                                              listToSpliceStx,
                                                              new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Form.Symbol("quasiquote")),
                                                                                           new Syntax(SyntaxList.FromIEnumerable(stxListArg.Skip<Syntax>(1)))),
                                                                     new SrcLoc())),
                                    stx.SrcLoc);

                } else {
                    // (cons (quasiquote slist) (quasiquote rest))
                    result = new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Form.Symbol("cons")),
                                                          new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Form.Symbol("quasiquote")),
                                                                                           stxListArg.ElementAt<Syntax>(0)),
                                                                     new SrcLoc()),
                                                          new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Form.Symbol("quasiquote")),
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
        new ExpansionEnvironment(new Dictionary<Form.Symbol, Transformer>{
            {new Form.Symbol("and"), new Transformer((Func<Delegate, Form, Thunk?>) and_macro)},
            {new Form.Symbol("match"), new Transformer((Func<Delegate, Form, Thunk?>) match_macro)},
            {new Form.Symbol("match-syntax"), new Transformer((Func<Delegate, Form, Thunk?>) Builtins.match_syntax_macro)},
            {new Form.Symbol("quasiquote"), new Transformer((Func<Delegate, Form, Thunk?>) quasiquote_macro)},
            {new Form.Symbol("syntax-rules"), new Transformer((Func<Delegate, Form, Thunk?>) Builtins.SyntaxRules.macro)},
            }
        );

    public bool TryFindTransformer(Form.Symbol sym, [NotNullWhen(returnValue: true)] out Transformer? macro) {
        if (_dict.TryGetValue(sym, out Transformer? result)) {
            macro = result;
            return true;
        }
        macro = null;
        return false;
    }

    public void AddTransformer(Form.Symbol sym, Transformer transformer) {
        _dict[sym] = transformer;
    }

    private Dictionary<Form.Symbol, Transformer> _dict;

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
