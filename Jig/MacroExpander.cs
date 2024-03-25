using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;

namespace Jig;

public class MacroExpander {

    internal Scope TopLevelScope = new Scope();

    public MacroExpander() {

        Bindings = new Dictionary<Syntax.Identifier, Binding>();
        foreach (var sym in Program.TopLevel.Symbols) {
            // add all top level symbols to bindings
            var id = new Syntax.Identifier(sym, new SrcLoc()); // TODO: srcloc should refer to prelude file or wherever
            Syntax.AddScope(id, TopLevelScope);
            Bindings.Add(id, Binding.TopLevel);
        }

    }

    bool introduced = false;

    internal Dictionary<Syntax.Identifier, Binding> Bindings {get;}

    IEnumerable<Syntax.Identifier> FindCandidateIdentifiers(Syntax.Identifier id) {
        return Bindings.Keys.Where(i => i.Symbol == id.Symbol && i.ScopeSet.IsSubsetOf(id.ScopeSet));
    }

    bool TryResolve(Syntax.Identifier id, [NotNullWhen(returnValue: true)] out Binding? binding) {
        var candidates = FindCandidateIdentifiers(id);
        if (candidates.Count() == 0) {
            binding = null;
            return false;
        }
        Syntax.Identifier maxID = candidates.MaxBy<Syntax.Identifier, int>(i => i.ScopeSet.Count) ?? throw new Exception("impossible");
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



    public Syntax Expand(Syntax ast, ExpansionEnvironment ee) {
        bool foundMacro = false;
        if (!introduced) {
            Syntax.AddScope(ast, TopLevelScope); // introduce
            introduced = true;
        }
        do {
            Syntax save = ast;
            (foundMacro, ast) = Expand_1(ast, ee);
            // if (foundMacro) {
            //     Console.WriteLine($"{save} => {ast}");
            // }

        } while (foundMacro);
        return ast;
    }

    public (bool, Syntax) Expand_1(Syntax stx, ExpansionEnvironment ee) {
        // ast = ast is SyntaxObject stx ? SyntaxObject.ToDatum(stx) : ast;
        // TODO: rewrite this in a way that we don't have to remember to change it everytime a keyword is added
        switch (stx) {
            case Syntax.Identifier id: return (false, id);
            case Syntax.Literal lit: return (false, lit);
        }
        if (Syntax.E(stx) is SyntaxList stxList) {
            if (Expr.IsKeyword("quote", stx)) {
                return (false, stx);
            } else if (Expr.IsKeyword("lambda", stx)) {
                return ExpandLambda(stx.SrcLoc, stxList, ee);
            } else if (Expr.IsKeyword("if", stx)) {
                return ExpandIf(stx.SrcLoc, stxList, ee);
            } else if (Expr.IsKeyword("define", stx)) {
                return ExpandDefine(stx.SrcLoc, stxList, ee);
            } else if (Expr.IsKeyword("begin", stx)) {
                return ExpandBegin(stx.SrcLoc, stxList, ee);
            } else if (Expr.IsKeyword("set!", stx)) {
                return ExpandSet(stx.SrcLoc, stxList, ee);
            } else if (Expr.IsKeyword("define-syntax", stx)) {
                return ExpandDefineSyntax(stx.SrcLoc, stxList, ee);
            } else {
                return ExpandApplication(stx, stxList, ee);
            }
        } else {
            return (false, stx);
        }
    }

    private (bool, Syntax) ExpandDefineSyntax(SrcLoc srcLoc, SyntaxList stxList, ExpansionEnvironment ee)
    {
        Syntax.Identifier id = stxList.ElementAt<Syntax>(1) as Syntax.Identifier ?? throw new Exception() ;
        // Scope newScope = new Scope();
        // Syntax.AddScope(id, newScope);
        Bindings.Add(id, new Binding());
        Syntax shouldBeLambdaExpr = stxList.ElementAt<Syntax>(2);
        // Syntax.AddScope(shouldBeLambdaExpr, newScope);
        if (!Expr.IsKeyword("lambda", shouldBeLambdaExpr)) {
            throw new Exception($"define-syntax: expected 2nd argument to be a transformer. Got {stxList.ElementAt<Syntax>(2)}");
        }
        Syntax expandedLambdaExpr = Expand(stxList.ElementAt<Syntax>(2), ee);
        Transformer transformer = EvaluateTransformer(expandedLambdaExpr);
        ee.AddTransformer(id.Symbol, transformer);
        return (false, new Syntax(List.NewList(new Syntax.Identifier(new Expr.Symbol("quote"), new SrcLoc()), id), srcLoc));

    }

    private Transformer EvaluateTransformer(Syntax lambdaExprSyntax) {
        Procedure procedure = Program.EvalNonCPS(lambdaExprSyntax) as Procedure ??
            throw new Exception($"define-syntax: second argument should be evaluate to a transformer.");
        return new Transformer(procedure.Value as Func<Delegate, Expr, Thunk> ??
            throw new Exception($"define-syntax: second argument should be a transformer (got {procedure.Value})"));
    }


    private (bool, Syntax) ExpandApplication(Syntax stx, SyntaxList stxList, ExpansionEnvironment ee)
    {
        if (stxList.ElementAt<Syntax>(0) is Syntax.Identifier id && ee.TryFindTransformer(id.Symbol, out Transformer? macro)) {
                List list = stxList.Rest;
                Scope macroExpansionScope = new Scope();
                Syntax.AddScope(stx, macroExpansionScope);
                Syntax output = macro.Apply(stx);
                var result = (true, output);
                Syntax.ToggleScope(result.Item2, macroExpansionScope);
                // Console.WriteLine($"{stx} => {output}");
                return result;
        } else {
                return ExpandSequence(stx.SrcLoc, stxList, ee);
        }
    }

    private (bool, Syntax) ExpandSequence(SrcLoc srcLoc, SyntaxList stxList, ExpansionEnvironment ee) {
                List<Syntax> xs = new List<Syntax>();
                bool foundMacro = false;
                foreach (var x in stxList) {
                    (bool foundMacroInBodyExpr, Syntax bodyExpr) = Expand_1(x, ee);
                    if (foundMacroInBodyExpr) {
                        foundMacro = true;
                    }
                    xs.Add(bodyExpr);
                }
                return (foundMacro, new Syntax(SyntaxList.FromIEnumerable(xs), srcLoc));

    }

    private (bool, Syntax) ExpandSet(SrcLoc srcLoc, SyntaxList stxList, ExpansionEnvironment ee)
    {
        bool foundMacro = false;
        List<Syntax> xs = new List<Syntax>();
        Debug.Assert(stxList.Count<Syntax>() == 3);
        xs.Add(stxList.ElementAt<Syntax>(0));
        xs.Add(stxList.ElementAt<Syntax>(1));
        foreach (var x in stxList.Skip<Syntax>(2)) {
            (bool foundMacroInBodyExpr, Syntax bodyExpr) = Expand_1(x, ee);
            if (foundMacroInBodyExpr) {
                foundMacro = true;
            }
            xs.Add(bodyExpr);
        }
        return (foundMacro, new Syntax(SyntaxList.FromIEnumerable(xs), srcLoc));
    }

    private (bool, Syntax) ExpandBegin(SrcLoc srcLoc, SyntaxList stxList, ExpansionEnvironment ee)
    {
        return ExpandSequence(srcLoc, stxList, ee);
    }

    private (bool, Syntax) ExpandDefine(SrcLoc srcLoc, SyntaxList stxList, ExpansionEnvironment ee)
    {
        bool foundMacro = false;
        List<Syntax> xs = new List<Syntax>();
        Debug.Assert(stxList.Count<Syntax>() == 3);
        xs.Add(stxList.ElementAt<Syntax>(0));
        xs.Add(stxList.ElementAt<Syntax>(1));
        Syntax.Identifier id = stxList.ElementAt<Syntax>(1) as Syntax.Identifier
            ?? throw new Exception($"ExpandDefine: expected first argument to be identifier. Got {stxList.ElementAt<Syntax>(1)}");
        // var newScope = new Scope();
        // Syntax.AddScope(id, newScope);
        if (!Bindings.ContainsKey(id)) {
            Bindings.Add(id, new Binding());
        }
        var x = stxList.ElementAt<Syntax>(2);
        // Syntax.AddScope(x, newScope);
        (bool foundMacroInBodyExpr, Syntax bodyExpr) = Expand_1(x, ee);
        if (foundMacroInBodyExpr) {
            foundMacro = true;
        }
        xs.Add(bodyExpr);
        return (foundMacro, new Syntax(SyntaxList.FromIEnumerable(xs), srcLoc));
    }

    private (bool, Syntax) ExpandIf(SrcLoc srcLoc, SyntaxList stxList, ExpansionEnvironment ee)
    {
        bool foundMacro = false;
        List<Syntax> xs = new List<Syntax>();
        Debug.Assert(stxList.Count<Syntax>() >= 3);
        xs.Add(stxList.ElementAt<Syntax>(0));
        foreach (var x in stxList.Skip<Syntax>(1)) {
            (bool foundMacroInBodyExpr, Syntax bodyExpr) = Expand_1(x, ee);
            if (foundMacroInBodyExpr) {
                foundMacro = true;
            }
            xs.Add(bodyExpr);
        }
        return (foundMacro, new Syntax(SyntaxList.FromIEnumerable(xs), srcLoc));
    }

    private (bool, Syntax) ExpandLambda(SrcLoc srcLoc, SyntaxList stxList, ExpansionEnvironment ee) {
        // TODO: Parser should produce a lambdaExpr Expr that is a type of List.NonEmpty
        bool foundMacro = false;
        List<Syntax> xs = new List<Syntax>();
        // below assert breaks a lot of tests having to do with multiple values
        // Debug.Assert(astAsList.Count() > 3);
        xs.Add(stxList.ElementAt<Syntax>(0)); // lamdbda keyword
        var newScope = new Scope();
        var parameters = stxList.ElementAt<Syntax>(1);
        Syntax.AddScope(parameters, newScope);
        // create a new binding for each parameter
        if (Syntax.E(parameters) is SyntaxList psStxList) {
            foreach(var stx in psStxList) {
                Syntax.Identifier id = stx as Syntax.Identifier ?? throw new Exception($"ExpandLambda: expected parameters to be identifiers, but got {stx}");
                if (!Bindings.ContainsKey(id)){
                    Bindings.Add(id, new Binding());
                }
            }
        } else if (Syntax.E(parameters) is IPair pair) {
            while (pair.Cdr is IPair cdrPair) {
                if (pair.Car is Syntax.Identifier ident) {
                   if (!Bindings.ContainsKey(ident)) {
                       Bindings.Add(ident, new Binding());
                   }
                }
                pair = cdrPair;
            }
            if (pair.Cdr is Syntax.Identifier id) {
                if (!Bindings.ContainsKey(id)) {
                    Bindings.Add(id, new Binding());
                }
            }
        } else if (parameters is Syntax.Identifier psId) {
                Bindings.Add(psId, new Binding());
        } else if (Syntax.E(parameters) is List.NullType) {
            
        } else {
            throw new Exception($"ExpandLambda: expected parameters to be list or identifier, got {Syntax.E(parameters)}");
        }
        xs.Add(parameters);
        foreach (var x in stxList.Skip<Syntax>(2)) {
            Syntax.AddScope(x, newScope);
            (bool foundMacroInBodyExpr, Syntax bodyExpr) = Expand_1(x, ee);
            if (foundMacroInBodyExpr) {
                foundMacro = true;
            }
            xs.Add(bodyExpr);
        }
        return (foundMacro, new Syntax(SyntaxList.FromIEnumerable(xs), srcLoc));
    }

}

public class ExpansionEnvironment {

    public ExpansionEnvironment(Dictionary<Expr.Symbol, Transformer> dict) {
        _dict = dict;
    }

    // private static Thunk or_macro(Delegate k, List args) {
    //     Syntax result;
    //     if (args.Count() == 0) {
    //         result = new Syntax(new Expr.Boolean(false), new SrcLoc());
    //         return Continuation.ApplyDelegate(k, result);
    //     }
    //     SyntaxList stxList = args as SyntaxList ?? throw new Exception($"in or_macro: expected args to be SyntaxList");
    //     Syntax first = stxList.ElementAt<Syntax>(0);
    //     result = new Syntax(
    //         SyntaxList.FromParams(new Syntax.Identifier(new Expr.Symbol("if"), new SrcLoc()),
    //                                 first,
    //                                 first,
    //                                 new Syntax(
    //                                 SyntaxList.FromIEnumerable(new List<Syntax>{
    //                                     new Syntax.Identifier(new Expr.Symbol("or"), new SrcLoc())
    //                                                                         }.Concat<Syntax>(stxList.Skip<Syntax>(1))),
    //                                 new SrcLoc())),
    //         new SrcLoc()); // TODO: should get whole sytax with srcLoc in args and use it
    //     return Continuation.ApplyDelegate(k, result);
    // }

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
            new SrcLoc()); // TODO: should get whole sytax with srcLoc in args and use it
        return Continuation.ApplyDelegate(k, result);

    }

    public static ExpansionEnvironment Default {get;} =
        new ExpansionEnvironment(new Dictionary<Expr.Symbol, Transformer>{
            {new Expr.Symbol("and"), new Transformer((Func<Delegate, Expr, Thunk>) and_macro)},
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
    public static Binding TopLevel {get;} = new Binding();
}
