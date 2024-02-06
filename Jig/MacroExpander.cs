using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;

namespace Jig;

public class MacroExpander {

    public Syntax Expand(Syntax ast, ExpansionEnvironment ee) {
        bool foundMacro = false;
        do {
            Syntax save = ast;
            (foundMacro, ast) = Expand_1(ast, ee);
            if (foundMacro) {
                Console.WriteLine($"Expand: I expanded a macro! ({save} => {ast}). Better check to see if it generated another!");
            }

        } while (foundMacro);
        return ast;
    }

    private (bool, Syntax) Expand_1(Syntax stx, ExpansionEnvironment ee) {
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
            } else {
                return ExpandApplication(stx.SrcLoc, stxList, ee);
            }
        } else {
            return (false, stx);
        }
    }

    private (bool, Syntax) ExpandApplication(SrcLoc srcLoc, SyntaxList stxList, ExpansionEnvironment ee)
    {
        if (stxList.ElementAt<Syntax>(0) is Syntax.Identifier id && ee.TryFindMacro(id.Symbol, out Macro? macro)) {
                List list = stxList.Rest;
                return (true, macro.Apply(list)); // TODO: macros should take whole stx not just args
        } else {
                return ExpandSequence(srcLoc, stxList, ee);
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
        foreach (var x in stxList.Skip<Syntax>(2)) {
            (bool foundMacroInBodyExpr, Syntax bodyExpr) = Expand_1(x, ee);
            if (foundMacroInBodyExpr) {
                foundMacro = true;
            }
            xs.Add(bodyExpr);
        }
        return (foundMacro, new Syntax(SyntaxList.FromIEnumerable(xs), srcLoc));
    }

    private (bool, Syntax) ExpandIf(SrcLoc srcLoc, SyntaxList stxList, ExpansionEnvironment ee)
    {
        bool foundMacro = false;
        List<Syntax> xs = new List<Syntax>();
        Debug.Assert(stxList.Count<Syntax>() == 4);
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

}

public class ExpansionEnvironment {

    public ExpansionEnvironment(Dictionary<Expr.Symbol, Macro> dict) {
        _dict = dict;
    }

    private static Thunk or_macro(Delegate k, List args) {
        Syntax result;
        if (args.Count() == 0) {
            result = new Syntax(new Expr.Boolean(false), new SrcLoc());
            return Continuation.ApplyDelegate(k, result);
        }
        SyntaxList stxList = args as SyntaxList ?? throw new Exception($"in or_macro: expected args to be SyntaxList");
        Syntax first = stxList.ElementAt<Syntax>(0);
        result = new Syntax(
            SyntaxList.FromParams(new Syntax(new Expr.Symbol("if"), new SrcLoc()),
                                    first,
                                    first,
                                    new Syntax(
                                    SyntaxList.FromIEnumerable(new List<Syntax>{
                                        new Syntax.Identifier(new Expr.Symbol("or"), new SrcLoc())
                                                                            }.Concat<Syntax>(stxList.Skip<Syntax>(1))),
                                    new SrcLoc())),
            new SrcLoc()); // TODO: should get whole sytax with srcLoc in args and use it
        return Continuation.ApplyDelegate(k, result);
    }

    public static ExpansionEnvironment Default {get;} =
        new ExpansionEnvironment(new Dictionary<Expr.Symbol, Macro>{
            {new Expr.Symbol("or"), new Macro((Builtin) or_macro)},
            }
        );

    public bool TryFindMacro(Expr.Symbol sym, [NotNullWhen(returnValue: true)] out Macro? macro) {
        if (_dict.TryGetValue(sym, out Macro? result)) {
            macro = result;
            return true;
        }
        macro = null;
        return false;
    }

    private Dictionary<Expr.Symbol, Macro> _dict;

}
