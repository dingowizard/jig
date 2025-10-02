using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using Jig;
using Jig.Expansion;

namespace DLR;

public class MacroExpander {

    internal Scope TopLevelScope = new Scope();

    public MacroExpander() {

        _bindings = new Dictionary<Identifier, Parameter>();
    }


    public Parameter[] Bindings => _bindings.Values.ToArray();

    public void AddBinding(Identifier id, Parameter parameter) {
        if (!_bindings.TryAdd(id, parameter)) {
            Console.Error.WriteLine($"Expander.AddBinding: Duplicate binding '{id}'");
            foreach (var kvp in _bindings) {
                if (kvp.Key.Symbol.Name == id.Symbol.Name && kvp.Key.ScopeSet == id.ScopeSet) {
                    Console.Error.WriteLine($"\ttrying to add {id.Symbol} which has scopeset \n\t\t{string.Join(", ", id.ScopeSet)}");
                    Console.Error.WriteLine($"\tBut there is a key with the same name with scopeset \n\t\t{string.Join(", ", kvp.Key.ScopeSet)}");
                    Console.Error.WriteLine($"\tThey have the same hash code? {kvp.Key.GetHashCode() == id.GetHashCode()}");
                    Console.Error.WriteLine($"\tThey the same object? {ReferenceEquals(id, kvp.Key)}");

                }
            }

            throw new Exception();
        }
    }

    private Dictionary<Identifier, Parameter> _bindings {get;}

    IEnumerable<Identifier> FindCandidateIdentifiers(Identifier id) {
        IEnumerable<Identifier> sameName = _bindings.Keys.Where(i => i.Symbol.Name == id.Symbol.Name);
        // if (id.Symbol.Name == "y") {
        //     Console.WriteLine($"The search id -- {id} -- has the following scope sets: {string.Join(',', id.ScopeSet)}");
        //     Console.WriteLine($"Found {sameName.Count()} bindings with same name.");
        //     foreach (var b in sameName) {
        //         Console.WriteLine($"\tscope sets: {string.Join(',', b.ScopeSet)}");
        //     }
        // }
        var result = sameName.Where(i => i.ScopeSet.IsSubsetOf(id.ScopeSet));
        return result;
    }

    internal bool TryResolve(Identifier id, [NotNullWhen(returnValue: true)] out Parameter? binding) {
        var candidates = FindCandidateIdentifiers(id);
        var identifiers = candidates as Identifier[] ?? candidates.ToArray();
        // if (id.Symbol.Name == "y") {
        //     Console.WriteLine($"TryResolve: found {identifiers.Length} candidates for 'y' at {id.SrcLoc} in {this.GetHashCode()}");
        // }
        if (identifiers.Length == 0) {
            binding = null;
            return false;
        }
        #pragma warning disable CS8600
        Identifier maxID = identifiers.MaxBy(i => i.ScopeSet.Count);// ?? throw new Exception("impossible");
        Debug.Assert(maxID is not null);
        #pragma warning restore CS8600
        CheckUnambiguous(maxID, identifiers);
        binding = _bindings[maxID];
        return true;
    }

    static void CheckUnambiguous(Identifier maxID, IEnumerable<Identifier> candidates) {
        // TODO: understand this better
        foreach (var candidate in candidates) {
            if (!candidate.ScopeSet.IsSubsetOf(maxID.ScopeSet)) {
                throw new Exception($"ambiguous : {maxID}");
            }
        }

    }

    public  ParsedForm Expand(Syntax stx, ExpansionEnvironment ee, bool definesAllowed = true, bool once = false) {
        // Console.WriteLine($"Expanding {stx}");
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
            if (SchemeValue.IsKeyword("define-syntax", stx)) {
                return ExpandDefineSyntax(stx.SrcLoc, stxList, ee);
            } else {
                return ExpandApplication(stx, stxList, ee, once);
            }
        } else {
            throw new Exception($"Expand: unhandled syntax {stx}, a {stx.GetType()} @ {stx.SrcLoc}");
        }
    }


    private  ParsedForm ExpandDefineSyntax(SrcLoc? srcLoc, SyntaxList stxList, ExpansionEnvironment ee)
    {
        Identifier id = stxList.ElementAt<Syntax>(1) as Identifier ?? throw new Exception() ;
        // Console.WriteLine($"define-syntax: {id}");
        var binding = new Parameter(id.Symbol, ee.ScopeLevel, ee.VarIndex++, id.SrcLoc);
        _bindings.Add(id, binding);
        // TODO: use ParsedLambda.TryParse? (for vars in set! and define as well?)
        ParsedLambda expandedLambdaExpr = Expand(stxList.ElementAt<Syntax>(2), ee) as ParsedLambda ?? throw new Exception($"define-syntax: expected 2nd argument to be a transformer. Got {stxList.ElementAt<Syntax>(2)}");
        Transformer transformer = EvaluateTransformer(expandedLambdaExpr); // TODO: should this part actually happen at runtime? then eval would need expansion env
        ee.AddTransformer(id.Symbol, transformer);
        Syntax result = new Syntax(SyntaxList.FromParams(new Identifier(new Symbol("quote")), id), srcLoc);
        if (ParsedLiteral.TryParse(result, out ParsedLiteral? lit)) {
            return lit;
        } else {
            throw new Exception($"ExpandDefineSyntax: result should be a quoted symbol. Got {result}");
        }

    }

    private static Transformer EvaluateTransformer(ParsedLambda lambdaExprSyntax) {
        Procedure procedure = Program.EvalNonCPSNoExpand(lambdaExprSyntax) as Procedure ??
            throw new Exception($"define-syntax: second argument should evaluate to a transformer.");
        return new Transformer(procedure.Value as Func<Delegate, SchemeValue, Thunk> ??
            throw new Exception($"define-syntax: second argument should be a transformer (got {procedure.Value})"));
    }


    private  ParsedForm ExpandApplication(Syntax stx, SyntaxList stxList, ExpansionEnvironment ee, bool once = false)
    {
        if (stxList.ElementAt<Syntax>(0) is Identifier id && ee.TryFindTransformer(id.Symbol, out Transformer? transformer)) {
            Scope macroExpansionScope = new Scope();
            
            Syntax.AddScope(stx, macroExpansionScope);
            // Console.WriteLine($"macro application: {stx}");
            Syntax output = transformer.Apply(stx);
            // var result = output;
            // Console.WriteLine($"toggling scope on {output}");
            Syntax.ToggleScope(output, macroExpansionScope);
            // Console.WriteLine($"\t => {output}");
            if (once) {
                if (ParsedQuoteSyntax.TryParse(new Syntax(SyntaxList.FromParams(new Identifier(new Symbol("quote-syntax")), output)),
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

    private  ParsedApplication ExpandList(SrcLoc? srcLoc, SyntaxList stxList, ExpansionEnvironment ee) {
        System.Collections.Generic.List<ParsedForm> xs = [];
        foreach (Syntax x in stxList.Cast<Syntax>()) {
            ParsedForm bodyExpr = Expand(x, ee, definesAllowed: false);
            xs.Add(bodyExpr);
        }
        return new ParsedApplication(xs, srcLoc);
    }


}

public class ExpansionEnvironment {

    public ExpansionEnvironment(Dictionary<Symbol, Transformer> dict) {
        _dict = dict;
    }

    private ExpansionEnvironment(Dictionary<Symbol, Transformer> dict, int scopeLevel) {
        ScopeLevel = scopeLevel;
        _dict = dict;
    }
    public ExpansionEnvironment Extend() {
        return new ExpansionEnvironment(this._dict, ScopeLevel + 1);
    }

    public int ScopeLevel { get; }

    public int VarIndex = 0;

    private static Thunk? match_macro(Delegate k, SchemeValue arg) {
        Syntax stx = arg as Syntax ?? throw new Exception($"match: expected syntax, got {arg.GetType()}");
        SyntaxList stxList = Syntax.E(stx) as SyntaxList ?? throw new Exception("match: syntax should expand to list");
        if (stxList.Count<Syntax>() < 2) throw new Exception(); // TODO: this should be a syntax error
        Syntax expr = stxList.ElementAt<Syntax>(1);
        Identifier x = new (new Symbol("x"));
        Syntax result = new Syntax(
            SyntaxList.FromParams(
                new Syntax(
                    SyntaxList.FromParams(
                        new Identifier(new Symbol("lambda")),
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
        var enumerable = clauses as Syntax[] ?? clauses.ToArray();
        Debug.Assert(enumerable.Length != 0);
        var elseBranch =
            enumerable.Length == 1 ?
            new Syntax(SyntaxList.FromParams(
                new Identifier(new Symbol("error")),
                new Syntax.Literal(new Jig.String("match: couldn't find a match."))
            )) : 
            new Syntax(MakeIfs(x, enumerable.Skip(1)));
          
        var thisClause =
            Syntax.E(enumerable.ElementAt(0)) as SyntaxList.NonEmpty ?? throw new Exception(); // TODO: should be syntax error
        return SyntaxList.FromParams(
            new Identifier(new Symbol("if")),
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
                    new Identifier(new Symbol("lambda")),
                    ParamsForLambdaForMatchClauseThen(pattern)
                }.Concat(bodies.Cast<Syntax>()).ToSyntaxList())
            }.ToSyntaxList();
        SyntaxList args = Flatten(ArgsForLambdaForMatchClauseThen(x, pattern));
        result = result.Concat<Syntax>(args).ToSyntaxList();
        return new Syntax(result);
    }


    private static SyntaxList Flatten(ISchemeValue x) {

        switch (x) {
            case IEmptyList: return SyntaxList.Null;
            case Syntax stx: return SyntaxList.FromParams(stx);
            case IPair pair:
                return (SyntaxList)Flatten(pair.Car).Append(Flatten(pair.Cdr));
            default: throw new Exception($"Flatten: unhandled case {x} a {x.GetType()}");
        }

    }

    private static List ArgsForLambdaForMatchClauseThen(Syntax x, SyntaxList patterns) {
        if (patterns.ElementAt<Syntax>(0) is Identifier id && id.Symbol.Name == "quote") {
            return SyntaxList.Null;
        }
        if (patterns.ElementAt<Syntax>(0) is Identifier q && q.Symbol.Name == "quote-syntax") {
            return SyntaxList.Null;
        }
        System.Collections.Generic.List<ISchemeValue> result = [];
        for (int i = 0; i < patterns.Count<Syntax>(); i++){
            result.Add(ArgsForLambdaForMatchClauseThen(NthElementOfList(i, x), patterns.ElementAt<Syntax>(i)));
        }
        return result.ToJigList();

    }

    private static ISchemeValue ArgsForLambdaForMatchClauseThen(Syntax x, Syntax pattern)
    {
        return Syntax.E(pattern) switch
        {
            List.Empty => SyntaxList.Null,
            Number => SyntaxList.Null,
            Bool => SyntaxList.Null,
            Symbol => x,
            SyntaxList stxList => ArgsForLambdaForMatchClauseThen(x, stxList),
            IPair pair => ArgsForLambdaForMatchClauseThen(x, pair),
            _ => throw new NotImplementedException(),
        };
    }
    private static ISchemeValue ArgsForLambdaForMatchClauseThen(Syntax x, IPair pair) {
        return Pair.Cons(
                ArgsForLambdaForMatchClauseThen(
                    x: new Syntax(SyntaxList.FromParams(
                        new Identifier(new Symbol("car")),
                        x)),
                    pattern: pair.Car),
                ArgsForLambdaForMatchClauseThen(
                    x: new Syntax(SyntaxList.FromParams(
                        new Identifier(new Symbol("cdr")),
                        x)),
                    pattern: pair.Cdr));
    }

    private static ISchemeValue ArgsForLambdaForMatchClauseThen(Syntax x, ISchemeValue pattern) {
        if (pattern is Syntax stx) {
            return ArgsForLambdaForMatchClauseThen(x, stx);
        } else if (pattern is IPair p) {
            return ArgsForLambdaForMatchClauseThen(x, p);
        } else {
            throw new NotImplementedException();
        }
    }

    private static ISchemeValue ParamsFromPattern(Syntax pattern) {
        switch (Syntax.E(pattern)) {
            case Number: return List.Null;
            case Bool: return List.Null;
            case List.Empty: return List.Null;
            case Symbol: return pattern;
            case SyntaxList stxList:
                if (SchemeValue.IsKeyword("quote", pattern)) {
                    return List.Null;
                }
                if (SchemeValue.IsKeyword("quote-syntax", pattern)) {
                    return List.Null;
                }
                System.Collections.Generic.List<ISchemeValue> res = [];
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

    private static ISchemeValue ParamsFromPattern(ISchemeValue schemeValue) {
        if (schemeValue is Syntax stx) {
            return ParamsFromPattern(stx);
        } else if (schemeValue is IPair pair) {
            return ParamsFromPattern(pair);
        } else {
            throw new NotImplementedException();
        }

    }

    private static ISchemeValue ParamsFromPattern(IPair pair) {
        return Pair.Cons(ParamsFromPattern(pair.Car), ParamsFromPattern(pair.Cdr));
    }

    private static Syntax ParamsForLambdaForMatchClauseThen(Syntax pattern) {
        var parameters = ParamsFromPattern(pattern);
        return new Syntax(Flatten(parameters));
    }

    private static Syntax MakeConditionForMatchClause(Syntax x, SyntaxList pattern) {
        if (pattern.ElementAt<Syntax>(0) is Identifier id && id.Symbol.Name == "quote") {
            return new Syntax(
                SyntaxList.FromParams(
                    new Identifier(new Symbol("eqv?")),
                    x,
                    new Syntax(pattern)));
        }
        if (pattern.ElementAt<Syntax>(0) is Identifier q && q.Symbol.Name == "quote-syntax") {
            return new Syntax(
                SyntaxList.FromParams(
                    new Identifier(new Symbol("eqv?")),
                    x,
                    new Syntax(pattern)));
        }
        System.Collections.Generic.List<Syntax> firstPart = [
            new Identifier(new Symbol("and")),
            new Syntax(SyntaxList.FromParams(
                new Identifier(new Symbol("list?")),
                x
            )),
            new Syntax(SyntaxList.FromParams(
                new Identifier(new Symbol("=")),
                    new Syntax(SyntaxList.FromParams(
                        new Identifier(new Symbol("length")),
                        x
                    )),
                    new Syntax.Literal(new Integer(pattern.Count<Syntax>()))))
        ];
        var secondPart = new System.Collections.Generic.List<Syntax>();
        for(int i = 0; i < pattern.Count<Syntax>(); i++) {
            secondPart.Add(MakeConditionForMatchClause(NthElementOfList(i, x), pattern.ElementAt<Syntax>(i)));
        }
        return new Syntax(SyntaxList.FromIEnumerable(firstPart.Concat(secondPart)));
    }

    private static Syntax NthElementOfList(int i, Syntax x) {
        while (i > 0) {
            x = new Syntax(SyntaxList.FromParams(new Identifier(new Symbol("cdr")), x));
            i--;
        }
        return new Syntax(SyntaxList.FromParams(new Identifier(new Symbol("car")), x));
    }

    private static Syntax MakeConditionForMatchClause(Syntax x, ISchemeValue car, ISchemeValue cdr) {
        // TODO: Pair<Syntax> type?
        Syntax carSyntax = car as Syntax ?? throw new Exception();
        if (cdr is Syntax cdrSyntax) {
            return new Syntax(
                SyntaxList.FromParams(
                    new Identifier(new Symbol("and")),
                    new Syntax(SyntaxList.FromParams(
                        new Identifier(new Symbol("pair?")),
                        x
                    )),
                    MakeConditionForMatchClause(new Syntax(SyntaxList.FromParams(new Identifier(new Symbol("car")), x)), carSyntax),
                    MakeConditionForMatchClause(new Syntax(SyntaxList.FromParams(new Identifier(new Symbol("cdr")), x)), cdrSyntax))
            );
        } else {
            if (cdr is IPair p) {
                return new Syntax(
                    SyntaxList.FromParams(
                        new Identifier(new Symbol("and")),
                        new Syntax(SyntaxList.FromParams(
                            new Identifier(new Symbol("pair?")),
                            x
                        )),
                        MakeConditionForMatchClause(new Syntax(SyntaxList.FromParams(new Identifier(new Symbol("car")), x)), carSyntax),
                        MakeConditionForMatchClause(
                            new Syntax(
                                SyntaxList.FromParams(new Identifier(new Symbol("cdr")), x)),
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
                new Identifier(new Symbol("null?")),
                x
        ));
    }

    private static Syntax MakeConditionForMatchClause(Syntax x, Syntax pattern) {
        return Syntax.E(pattern) switch
        {
            Number => MakeNumEqTest(pattern, x),
            Bool => new Syntax(SyntaxList.FromParams(new Identifier(new Symbol("eqv?")), pattern, x)),
            Syntax => new Syntax(SyntaxList.FromParams(new Identifier(new Symbol("eqv?")), pattern, x)),
            List.Empty => MakeNullTest(x),
            Symbol => new Syntax.Literal(Bool.True),
            SyntaxList syntaxList => MakeConditionForMatchClause(x, syntaxList),
            IPair pair => MakeConditionForMatchClause(x, pair.Car, pair.Cdr),
            _ => throw new NotImplementedException($"x = {x.Print()} and pattern = {pattern.Print()}"),
        };
    }

    private static Syntax MakeNumEqTest(Syntax n, Syntax x)
    {
        return new Syntax(
            SyntaxList.FromParams(
                new Identifier(new Symbol("=")),
                x,
                n
        ));
    }

    private static Thunk? and_macro(Delegate k, SchemeValue x) {
        Syntax stx = x as Syntax ?? throw new Exception($"and: expected syntax, got {x.GetType()}");
        Syntax result;
        SyntaxList stxList = Syntax.E(stx) as SyntaxList ?? throw new Exception("and: syntax should expand to list");
        if (stxList.Count<Syntax>() == 1) { // E.g. (and)
            result = new Syntax.Literal(Bool.True);
            return Continuation.ApplyDelegate(k, result);
        }
        if (stxList.Count<Syntax>() == 2) { // Eg (and 1)
            result = stxList.ElementAt<Syntax>(1);
            return Continuation.ApplyDelegate(k, result);
        }
        Syntax first = stxList.ElementAt<Syntax>(1);
        result = new Syntax(
            SyntaxList.FromParams(new Identifier(new Symbol("if")),
                                    first,
                                    new Syntax(
                                    SyntaxList.FromIEnumerable(new System.Collections.Generic.List<Syntax>{
                                        new Identifier(new Symbol("and"))
                                                                            }.Concat(stxList.Skip<Syntax>(2))),

                                    new SrcLoc()),
                                    new Syntax.Literal(Bool.False)),
            stx.SrcLoc);
        return Continuation.ApplyDelegate(k, result);

    }

    private static Thunk? quasiquote_macro(Delegate k, SchemeValue x) {
        Syntax stx = x as Syntax ?? throw new Exception($"quasiquote: expected syntax, got {x.GetType()}");
        SyntaxList stxList = Syntax.E(stx) as SyntaxList ?? throw new Exception("quasiquote: syntax should expand to list");
        Syntax result;
        if (stxList.Count<Syntax>() != 2) { // Eg (quasiquote x)
            throw new Exception($"quasiquote: expected exactly one argument");
        }
        Syntax arg = stxList.ElementAt<Syntax>(1);
        ISchemeValue argE = Syntax.E(arg);
        if (argE is List.Empty ||
            argE is not IPair) {
            result = new Syntax(SyntaxList.FromParams(new Identifier(new Symbol("quote")),
                                                      arg),
                                stx.SrcLoc);
        } else if (argE is SyntaxList stxListArg) {
            if (SchemeValue.IsKeyword("unquote", arg)) { // (quasiquote (unquote x))
                if (stxListArg.Count<Syntax>() != 2) {
                    throw new Exception($"unquote: expected exactly one argument. Got {stxListArg.Count<Syntax>() - 1}");
                } else {
                    result = stxListArg.ElementAt<Syntax>(1);
                }
            } else if (Syntax.E(stxListArg.ElementAt<Syntax>(0)) is not SyntaxList) { // (quasiquote (x . rest)) where x is not a pair
                                                                       // => (cons (quote x) (quasiquote rest))
                result = new Syntax(SyntaxList.FromParams(new Identifier(new Symbol("cons")),
                                                          new Syntax(SyntaxList.FromParams(new Identifier(new Symbol("quote")),
                                                                                           stxListArg.ElementAt<Syntax>(0)),
                                                                     new SrcLoc()),
                                                          new Syntax(SyntaxList.FromParams(new Identifier(new Symbol("quasiquote")),
                                                                                           new Syntax(SyntaxList.FromIEnumerable(stxListArg.Skip<Syntax>(1)))),
                                                                     new SrcLoc())),
                                    stx.SrcLoc);

            } else if (Syntax.E(stxListArg.ElementAt<Syntax>(0)) is SyntaxList slist) { // (quasiquote ((car . cdr) . rest))
                if (SchemeValue.IsKeyword("unquote-splicing", stxListArg.ElementAt<Syntax>(0))) {
                    // (append (car (cdr slist) (quasiquote rest))
                    if (slist.Count<Syntax>() != 2) {
                        throw new Exception("unquote-splicing: expected exactly one argument.");
                    }
                    Syntax listToSpliceStx = slist.ElementAt<Syntax>(1);
                    // if (Syntax.E(listToSpliceStx) is not SyntaxList) {
                    //     throw new Exception("unquote-splicing: expected a list argument");
                    // }
                    result = new Syntax(SyntaxList.FromParams(new Identifier(new Symbol("append")),
                                                              listToSpliceStx,
                                                              new Syntax(SyntaxList.FromParams(new Identifier(new Symbol("quasiquote")),
                                                                                           new Syntax(SyntaxList.FromIEnumerable(stxListArg.Skip<Syntax>(1)))),
                                                                     new SrcLoc())),
                                    stx.SrcLoc);

                } else {
                    // (cons (quasiquote slist) (quasiquote rest))
                    result = new Syntax(SyntaxList.FromParams(new Identifier(new Symbol("cons")),
                                                          new Syntax(SyntaxList.FromParams(new Identifier(new Symbol("quasiquote")),
                                                                                           stxListArg.ElementAt<Syntax>(0)),
                                                                     new SrcLoc()),
                                                          new Syntax(SyntaxList.FromParams(new Identifier(new Symbol("quasiquote")),
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
internal static Thunk? syntax_rules_macro(Delegate k, SchemeValue arg) {
            if (arg is not Syntax stx) {
                throw new Exception($"syntax-rules: was passed {arg.Print()}, which is not a syntax object");
            }
            if (Syntax.E(stx) is not SyntaxList.NonEmpty stxList) {
                throw new Exception($"syntax-rules: was passed {stx.Print()}, which is not a syntax list");
            }
            if (stxList.Rest is not SyntaxList.NonEmpty macroArgs) {
                throw new Exception($"syntax-rules: expected subforms, but got none");
            }
            if (Syntax.E(macroArgs.First) is not SyntaxList literals) {
                if (Syntax.E(macroArgs.First) is not IEmptyList) {
                    throw new Exception($"syntax-rules: expected first subform to be a list, but got {Syntax.E(macroArgs.First).Print()}");
                }
                literals = SyntaxList.FromParams();

            }
            var clauses = new System.Collections.Generic.List<Tuple<SyntaxList.NonEmpty, Syntax>>();
            foreach (var clause in macroArgs.Rest.Cast<Syntax>()) {
                if (Syntax.E(clause) is SyntaxList.NonEmpty clauseStxList) {
                    if (Syntax.E(clauseStxList.First) is not SyntaxList.NonEmpty pattern) {
                        throw new Exception($"syntax-rules: malformed clause: {clause.Print()}. Pattern should be a list, but got {clauseStxList.First}");
                    }
                    if (clauseStxList.Rest is not SyntaxList.NonEmpty rest) {
                        throw new Exception($"syntax-rules: malformed clause: {clause.Print()}. Expected template.");
                    }
                    if (rest.Rest is not SyntaxList.Empty) {
                        throw new Exception($"syntax-rules: malformed clause: {clause.Print()}. Expected one template.");
                    }
                    clauses.Add(new Tuple<SyntaxList.NonEmpty,Syntax>(pattern, rest.First));

                } else {
                    throw new Exception($"syntax-rules: malformed clause: {clause.Print()}");
                }
            }
            var stxParam = NewSym("stx");
            var result = NewList( // (lambda (stx) ((lambda (x) ...) (syntax-e stx)))
                NewSym("lambda"),
                NewList(stxParam),
                NewList(
                    new SyntaxRules(literals.ToSyntaxList(), clauses).LambdaFromClauses(),
                    NewList(NewSym("syntax-e"), stxParam)));
            return Continuation.ApplyDelegate(k, Syntax.FromDatum(stx.SrcLoc, result));

        }

    public static ExpansionEnvironment Default {get;} =
        new ExpansionEnvironment(new Dictionary<Symbol, Transformer>{
            {new Symbol("and"), new Transformer((Func<Delegate, SchemeValue, Thunk?>) and_macro)},
            {new Symbol("match"), new Transformer((Func<Delegate, SchemeValue, Thunk?>) match_macro)},
            {new Symbol("match-syntax"), new Transformer((Func<Delegate, SchemeValue, Thunk?>) Builtins.match_syntax_macro)},
            {new Symbol("quasiquote"), new Transformer((Func<Delegate, SchemeValue, Thunk?>) quasiquote_macro)},
            {new Symbol("syntax-rules"), new Transformer((Func<Delegate, SchemeValue, Thunk?>) syntax_rules_macro)},
            }
        );

    public bool TryFindTransformer(Symbol sym, [NotNullWhen(returnValue: true)] out Transformer? macro) {
        if (_dict.TryGetValue(sym, out Transformer? result)) {
            macro = result;
            return true;
        }
        macro = null;
        return false;
    }

    public void AddTransformer(Symbol sym, Transformer transformer) {
        _dict[sym] = transformer;
    }

    private Dictionary<Symbol, Transformer> _dict;

}


