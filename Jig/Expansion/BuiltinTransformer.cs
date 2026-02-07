namespace Jig.Expansion;

public delegate Syntax BuiltinTransformerDelegate(Syntax stx);
public class BuiltinTransformer  : Transformer {

    public BuiltinTransformer(BuiltinTransformerDelegate transformerProcedure) {
        Procedure = transformerProcedure;
    }
    public BuiltinTransformerDelegate Procedure {get;}
    public override Syntax Transform(Syntax syntax) {
        return Procedure(syntax);
    }

    public static Syntax and(Syntax stx) {
        
        SyntaxList stxList = Syntax.E(stx) as SyntaxList ?? throw new Exception("and: syntax should expand to list");
        if (stxList.Count<Syntax>() == 1) { // E.g. (and)
            return new Syntax.Literal(Bool.True);
        }
        if (stxList.Count<Syntax>() == 2) { // Eg (and 1)
            return stxList.ElementAt<Syntax>(1);
        }
        Syntax first = stxList.ElementAt<Syntax>(1);
        return new Syntax(
            SyntaxList.FromParams(new Identifier(new Symbol("if")),
                first,
                new Syntax(
                    SyntaxList.FromIEnumerable(new System.Collections.Generic.List<Syntax>{
                        new Identifier(new Symbol("and"))
                    }.Concat(stxList.Skip<Syntax>(2))),

                    new SrcLoc()),
                new Syntax.Literal(Bool.False)),
            stx.SrcLoc);
    }

    public static Syntax quasiquote(Syntax stx) {
        
        SyntaxList stxList = Syntax.E(stx) as SyntaxList ?? throw new Exception("quasiquote: syntax should expand to list");
        if (stxList.Count<Syntax>() != 2) { // Eg (quasiquote x)
            throw new Exception($"quasiquote: expected exactly one argument");
        }
        Syntax arg = stxList.ElementAt<Syntax>(1);
        SchemeValue argE = Syntax.E(arg);
        if (argE is List.Empty ||
            argE is not IPair) {
            return new Syntax(SyntaxList.FromParams(new Identifier(new Symbol("quote")),
                                                      arg),
                                stx.SrcLoc);
        } else if (argE is SyntaxList stxListArg) {
            if (SchemeValue.IsKeyword("unquote", arg)) { // (quasiquote (unquote x))
                if (stxListArg.Count<Syntax>() != 2) {
                    throw new Exception($"unquote: expected exactly one argument. Got {stxListArg.Count<Syntax>() - 1}");
                } else {
                    return stxListArg.ElementAt<Syntax>(1);
                }
            } else if (Syntax.E(stxListArg.ElementAt<Syntax>(0)) is not SyntaxList) { // (quasiquote (x . rest)) where x is not a pair
                                                                       // => (cons (quote x) (quasiquote rest))
                return new Syntax(SyntaxList.FromParams(new Identifier(new Symbol("cons")),
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
                    return new Syntax(SyntaxList.FromParams(new Identifier(new Symbol("append")),
                                                              listToSpliceStx,
                                                              new Syntax(SyntaxList.FromParams(new Identifier(new Symbol("quasiquote")),
                                                                                           new Syntax(SyntaxList.FromIEnumerable(stxListArg.Skip<Syntax>(1)))),
                                                                     new SrcLoc())),
                                    stx.SrcLoc);

                } else {
                    // (cons (quasiquote slist) (quasiquote rest))
                    return new Syntax(SyntaxList.FromParams(new Identifier(new Symbol("cons")),
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
    }
    public static Syntax syntax_rules(Syntax stx) {
            if (Syntax.E(stx) is not SyntaxList.NonEmpty stxList) {
                throw new Exception($"syntax-rules: bad syntax {stx.Print()}");
            }
            if (stxList.Rest is not SyntaxList.NonEmpty macroArgs) {
                throw new Exception($"syntax-rules: expected sub-forms, but got none");
            }
            if (Syntax.E(macroArgs.First) is not SyntaxList literals) {
                if (Syntax.E(macroArgs.First) is not List { IsEmpty: true }) {
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
            var stxParam = Builtins.NewSym("stx");
            var result = Builtins.NewList( // (lambda (stx) ((lambda (x) ...) (syntax-e stx)))
                Builtins.NewSym("lambda"),
                Builtins.NewList(stxParam),
                Builtins.NewList(
                    new Builtins.SyntaxRules(literals.ToSyntaxList(), clauses).LambdaFromClauses(),
                    Builtins.NewList(Builtins.NewSym("syntax-e"), stxParam)));
            return Syntax.FromDatum(stx.SrcLoc, result);

        }
}
