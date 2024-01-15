using System.Diagnostics;

namespace Jig;

public class MacroExpander {

    public Expr Expand(Expr ast, ExpansionEnvironment ee) {
        bool foundMacro = false;
        do {
            (foundMacro, ast) = Expand_1(ast, ee);

        } while (foundMacro);
        return ast;
    }

    private (bool, Expr) Expand_1(Expr ast, ExpansionEnvironment ee) {
        ast = ast is SyntaxObject stx ? SyntaxObject.ToDatum(stx) : ast;
        // TODO: rewrite this in a way that we don't have to remember to change it everytime a keyword is added
        if (ast is List.NonEmpty listExpr) {
            if (Expr.IsKeyword("quote", listExpr)) {
                return (false, ast);
            } else if (Expr.IsKeyword("lambda", ast)) {
                return ExpandLambda(listExpr, ee);
            } else if (Expr.IsKeyword("if", ast)) {
                return ExpandIf(listExpr, ee);
            } else if (Expr.IsKeyword("define", ast)) {
                return ExpandDefine(listExpr, ee);
            } else if (Expr.IsKeyword("begin", ast)){
                return ExpandBegin(listExpr, ee);
            } else if (Expr.IsKeyword("set!", ast)){
                return ExpandSet(listExpr, ee);
            } else {
                return ExpandApplication(listExpr, ee);
            }
        } else {
            return (false, ast);
        }
    }

    private (bool, Expr) ExpandApplication(List.NonEmpty listExpr, ExpansionEnvironment ee)
    {
        if (listExpr.ElementAt(0) is Expr.Symbol sym && ee.TryFindMacro(sym, out Macro macro)) {
                var list = List.ListFromEnumerable(listExpr.Skip(1));
                return (true, macro.Apply(list));
        } else {
                return ExpandSequence(listExpr, ee);
        }
    }

    private (bool, Expr) ExpandSequence(List.NonEmpty exprs, ExpansionEnvironment ee) {
                List<Expr> xs = new List<Expr>();
                bool foundMacro = false;
                foreach (var x in exprs) {
                    (bool foundMacroInBodyExpr, Expr bodyExpr) = Expand_1(x, ee);
                    if (foundMacroInBodyExpr) {
                        foundMacro = true;
                    }
                    xs.Add(bodyExpr);
                }
                return (foundMacro, List.ListFromEnumerable(xs));

    }

    private (bool, Expr) ExpandSet(List.NonEmpty astAsList, ExpansionEnvironment ee)
    {
        bool foundMacro = false;
        List<Expr> xs = new List<Expr>();
        Debug.Assert(astAsList.Count() == 3);
        xs.Add(astAsList.ElementAt(0));
        xs.Add(astAsList.ElementAt(1));
        foreach (var x in astAsList.Skip(2)) {
            (bool foundMacroInBodyExpr, Expr bodyExpr) = Expand_1(x, ee);
            if (foundMacroInBodyExpr) {
                foundMacro = true;
            }
            xs.Add(bodyExpr);
        }
        return (foundMacro, List.ListFromEnumerable(xs));
    }

    private (bool, Expr) ExpandBegin(List.NonEmpty ast, ExpansionEnvironment ee)
    {
        return ExpandSequence(ast, ee);
    }

    private (bool, Expr) ExpandDefine(List.NonEmpty astAsList, ExpansionEnvironment ee)
    {
        bool foundMacro = false;
        List<Expr> xs = new List<Expr>();
        Debug.Assert(astAsList.Count() == 3);
        xs.Add(astAsList.ElementAt(0));
        xs.Add(astAsList.ElementAt(1));
        foreach (var x in astAsList.Skip(2)) {
            (bool foundMacroInBodyExpr, Expr bodyExpr) = Expand_1(x, ee);
            if (foundMacroInBodyExpr) {
                foundMacro = true;
            }
            xs.Add(bodyExpr);
        }
        return (foundMacro, List.ListFromEnumerable(xs));
    }

    private (bool, Expr) ExpandIf(List.NonEmpty astAsList, ExpansionEnvironment ee)
    {
        bool foundMacro = false;
        List<Expr> xs = new List<Expr>();
        Debug.Assert(astAsList.Count() == 4);
        xs.Add(astAsList.ElementAt(0));
        foreach (var x in astAsList.Skip(1)) {
            (bool foundMacroInBodyExpr, Expr bodyExpr) = Expand_1(x, ee);
            if (foundMacroInBodyExpr) {
                foundMacro = true;
            }
            xs.Add(bodyExpr);
        }
        return (foundMacro, List.ListFromEnumerable(xs));
    }

    private (bool, Expr) ExpandLambda(List.NonEmpty astAsList, ExpansionEnvironment ee) {
        // TODO: Parser should produce a lambdaExpr Expr that is a type of List.NonEmpty
        bool foundMacro = false;
        List<Expr> xs = new List<Expr>();
        // below assert breaks a lot of tests having to do with multiple values
        // Debug.Assert(astAsList.Count() > 3);
        xs.Add(astAsList.ElementAt(0));
        xs.Add(astAsList.ElementAt(1));
        foreach (var x in astAsList.Skip(2)) {
            (bool foundMacroInBodyExpr, Expr bodyExpr) = Expand_1(x, ee);
            if (foundMacroInBodyExpr) {
                foundMacro = true;
            }
            xs.Add(bodyExpr);
        }
        return (foundMacro, List.ListFromEnumerable(xs));
    }

}

public class ExpansionEnvironment {

    public ExpansionEnvironment(Dictionary<Expr.Symbol, Macro> dict) {
        _dict = dict;
    }

    private static Thunk or_macro(Delegate k, List args) {
        Expr result;
        if (args.Count() == 0) {
            result = new Expr.Boolean(false);
        } else {
            Expr first = args.ElementAt(0);
            result = List.NewList(new Expr.Symbol("if"), first, first, List.ListFromEnumerable(new List<Expr>{new Expr.Symbol("or")}.Concat(args.Skip(1))));
        }
        return Continuation.ApplyDelegate(k, result);
    }

    public static ExpansionEnvironment Default {get;} =
        new ExpansionEnvironment(new Dictionary<Expr.Symbol, Macro>{
            {new Expr.Symbol("or"), new Macro((Builtin) or_macro)},
            }
        );

    public bool TryFindMacro(Expr.Symbol sym, out Macro macro) {
        return _dict.TryGetValue(sym, out macro);
    }

    private Dictionary<Expr.Symbol, Macro> _dict;

}
