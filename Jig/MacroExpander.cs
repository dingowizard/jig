using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;

namespace Jig;

public class MacroExpander {

    internal Scope TopLevelScope = new Scope();

    public MacroExpander() {

        Bindings = new Dictionary<Syntax.Identifier, Binding>();
        // TODO: we shouldn't do this every time the macro expander runs
        // foreach (var sym in Program.TopLevel.Symbols) {
        //     // add all top level symbols to bindings
        //     var id = new Syntax.Identifier(sym, new SrcLoc()); // TODO: srcloc should refer to prelude file or wherever
        //     Syntax.AddScope(id, TopLevelScope);
        //     id.Symbol.Binding = Binding.TopLevel;
        //     Bindings.Add(id, Binding.TopLevel);
        // }
    }

    internal Dictionary<Syntax.Identifier, Binding> Bindings {get;}

    IEnumerable<Syntax.Identifier> FindCandidateIdentifiers(Syntax.Identifier id) {
        IEnumerable<Syntax.Identifier> sameName = Bindings.Keys.Where(i => i.Symbol.Name == id.Symbol.Name);
        // if (id.Symbol.Name == "u") {
        //     Console.WriteLine($"The search id -- {id} -- has the following scope sets: {string.Join(',', id.ScopeSet)}");
        //     Console.WriteLine($"Found {sameName.Count()} bindings with same name.");
        //     foreach (var b in sameName) {
        //         Console.WriteLine($"\tscope sets: {string.Join(',', b.ScopeSet)}");
        //     }
        // }
        var result = sameName.Where(i => i.ScopeSet.IsSubsetOf(id.ScopeSet));
        return result;
    }

    bool TryResolve(Syntax.Identifier id, [NotNullWhen(returnValue: true)] out Binding? binding) {
        var candidates = FindCandidateIdentifiers(id);
        if (id.Symbol.Name == "u") {
            Console.WriteLine($"TryResolve: found {candidates.Count()} candidates for 'u' at {id.SrcLoc}");
        }
        if (candidates.Count() == 0) {
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

    void CheckUnambiguous(Syntax.Identifier maxID, IEnumerable<Syntax.Identifier> candidates) {
        // TODO: understand this better
        foreach (var candidate in candidates) {
            if (!candidate.ScopeSet.IsSubsetOf(maxID.ScopeSet)) {
                throw new Exception($"ambiguous : {maxID}");
            }
        }

    }

    public  Syntax Expand(Syntax stx, ExpansionEnvironment ee, bool once = false) {
        // TODO: rewrite this in a way that we don't have to remember to change it everytime a keyword is added
        switch (stx) {
            case Syntax.Identifier id:
                if(TryResolve(id, out Binding? binding)) {
                    id.Symbol.Binding = binding;
                    return id;
                } else {
                    return id;
                }
            case Syntax.Literal lit: return lit;
        }
        if (Syntax.E(stx) is SyntaxList stxList) {
            if (Expr.IsKeyword("quote", stx)) {
                return stx;
            } else if (Expr.IsKeyword("lambda", stx)) {
                return ExpandLambda(stx.SrcLoc, stxList, ee);
            } else if (Expr.IsKeyword("if", stx)) {
                return ExpandIf(stx.SrcLoc, stxList, ee);
            } else if (Expr.IsKeyword("define", stx)) {
                return ExpandDefine(stx.SrcLoc, stxList, ee);
            } else if (Expr.IsKeyword("set!", stx)) {
                return ExpandSet(stx.SrcLoc, stxList, ee);
            } else if (Expr.IsKeyword("define-syntax", stx)) {
                return ExpandDefineSyntax(stx.SrcLoc, stxList, ee);
            } else if (Expr.IsKeyword("syntax", stx)) {
                return stx;
            } else {
                return ExpandApplication(stx, stxList, ee, once);
            }
        } else {
            return stx;
        }
    }

    private  Syntax ExpandDefineSyntax(SrcLoc srcLoc, SyntaxList stxList, ExpansionEnvironment ee)
    {
        Syntax.Identifier id = stxList.ElementAt<Syntax>(1) as Syntax.Identifier ?? throw new Exception() ;
        id.Symbol.Binding = new Binding();
        Bindings.Add(id, id.Symbol.Binding);
        Syntax shouldBeLambdaExpr = stxList.ElementAt<Syntax>(2);
        if (!Expr.IsKeyword("lambda", shouldBeLambdaExpr)) {
            throw new Exception($"define-syntax: expected 2nd argument to be a transformer. Got {stxList.ElementAt<Syntax>(2)}");
        }
        Syntax expandedLambdaExpr = Expand(stxList.ElementAt<Syntax>(2), ee);
        Transformer transformer = EvaluateTransformer(expandedLambdaExpr);
        ee.AddTransformer(id.Symbol, transformer);
        return new Syntax(List.NewList(new Syntax.Identifier(new Expr.Symbol("quote"), new SrcLoc()), id), srcLoc);

    }

    private Transformer EvaluateTransformer(Syntax lambdaExprSyntax) {
        Procedure procedure = Program.EvalNonCPS(lambdaExprSyntax) as Procedure ??
            throw new Exception($"define-syntax: second argument should be evaluate to a transformer.");
        return new Transformer(procedure.Value as Func<Delegate, Expr, Thunk> ??
            throw new Exception($"define-syntax: second argument should be a transformer (got {procedure.Value})"));
    }


    private  Syntax ExpandApplication(Syntax stx, SyntaxList stxList, ExpansionEnvironment ee, bool once = false)
    {
        if (stxList.ElementAt<Syntax>(0) is Syntax.Identifier id && ee.TryFindTransformer(id.Symbol, out Transformer? transformer)) {

            List list = stxList.Rest;
            Scope macroExpansionScope = new Scope();
            Syntax.AddScope(stx, macroExpansionScope);
            Syntax output = transformer.Apply(stx);
            // var result = output;
            Syntax.ToggleScope(output, macroExpansionScope);
            // Console.WriteLine($"{stx} => {output}");
            if (once) {
                return output;
            } else {
                return Expand(output, ee);
            }
        } else {
            return ExpandSequence(stx.SrcLoc, stxList, ee);
        }
    }

    private  Syntax ExpandSequence(SrcLoc srcLoc, SyntaxList stxList, ExpansionEnvironment ee) {
                List<Syntax> xs = new List<Syntax>();
                foreach (var x in stxList) {
                    Syntax bodyExpr = Expand(x, ee);
                    xs.Add(bodyExpr);
                }
                return new Syntax(SyntaxList.FromIEnumerable(xs), srcLoc);

    }

    private  Syntax ExpandSet(SrcLoc srcLoc, SyntaxList stxList, ExpansionEnvironment ee)
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

    private  Syntax ExpandDefine(SrcLoc srcLoc, SyntaxList stxList, ExpansionEnvironment ee)
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

    private  Syntax ExpandIf(SrcLoc srcLoc, SyntaxList stxList, ExpansionEnvironment ee)
    {
        List<Syntax> xs = new List<Syntax>();
        // Debug.Assert(stxList.Count<Syntax>() >= 3);
        xs.Add(stxList.ElementAt<Syntax>(0)); // TODO: maybe simplify all these by ignoring keywords on expand symbol?
        foreach (var x in stxList.Skip<Syntax>(1)) {
            Syntax bodyExpr = Expand(x, ee);
            xs.Add(bodyExpr);
        }
        return new Syntax(SyntaxList.FromIEnumerable(xs), srcLoc);
    }

    private  Syntax ExpandLambda(SrcLoc srcLoc, SyntaxList stxList, ExpansionEnvironment ee) {
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

    private static Thunk and_macro(Delegate k, Expr x) {
        Syntax stx = x as Syntax ?? throw new Exception($"and: expected syntax, got {x.GetType()}");
        Syntax result;
        SyntaxList stxList = Syntax.E(stx) as SyntaxList ?? throw new Exception("and: syntax should expand to list");
        if (stxList.Count<Syntax>() == 1) { // E.g. (and)
            result = new Syntax(new Expr.Boolean(true), new SrcLoc());
            return Continuation.ApplyDelegate(k, result);
        }
        if (stxList.Count<Syntax>() == 2) { // Eg (and 1)
            result = stxList.ElementAt<Syntax>(1);
            return Continuation.ApplyDelegate(k, result);
        }
        Syntax first = stxList.ElementAt<Syntax>(1);
        result = new Syntax(
            SyntaxList.FromParams(new Syntax.Identifier(new Expr.Symbol("if"), new SrcLoc()),
                                    first,
                                    new Syntax(
                                    SyntaxList.FromIEnumerable(new List<Syntax>{
                                        new Syntax.Identifier(new Expr.Symbol("and"), new SrcLoc())
                                                                            }.Concat<Syntax>(stxList.Skip<Syntax>(2))),

                                    new SrcLoc()),
                                    new Syntax(new Expr.Boolean(false), new SrcLoc())),
            stx.SrcLoc);
        return Continuation.ApplyDelegate(k, result);

    }

    private static Thunk quasiquote_macro(Delegate k, Expr x) {
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
            result = new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Expr.Symbol("quote"), new SrcLoc()),
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
                result = new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Expr.Symbol("cons"), new SrcLoc()),
                                                          new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Expr.Symbol("quote"), new SrcLoc()),
                                                                                           stxListArg.ElementAt<Syntax>(0)),
                                                                     new SrcLoc()),
                                                          new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Expr.Symbol("quasiquote"), new SrcLoc()),
                                                                                           new Syntax(SyntaxList.FromIEnumerable(stxListArg.Skip<Syntax>(1)), new SrcLoc())),
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
                    result = new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Expr.Symbol("append"), new SrcLoc()),
                                                              listToSpliceStx,
                                                              new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Expr.Symbol("quasiquote"), new SrcLoc()),
                                                                                           new Syntax(SyntaxList.FromIEnumerable(stxListArg.Skip<Syntax>(1)), new SrcLoc())),
                                                                     new SrcLoc())),
                                    stx.SrcLoc);

                } else {
                    // (cons (quasiquote slist) (quasiquote rest))
                    result = new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Expr.Symbol("cons"), new SrcLoc()),
                                                          new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Expr.Symbol("quasiquote"), new SrcLoc()),
                                                                                           stxListArg.ElementAt<Syntax>(0)),
                                                                     new SrcLoc()),
                                                          new Syntax(SyntaxList.FromParams(new Syntax.Identifier(new Expr.Symbol("quasiquote"), new SrcLoc()),
                                                                                           new Syntax(SyntaxList.FromIEnumerable(stxListArg.Skip<Syntax>(1)), new SrcLoc())),
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
            {new Expr.Symbol("and"), new Transformer((Func<Delegate, Expr, Thunk>) and_macro)},
            {new Expr.Symbol("quasiquote"), new Transformer((Func<Delegate, Expr, Thunk>) quasiquote_macro)},
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
