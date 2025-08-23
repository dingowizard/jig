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
        IForm argE = Syntax.E(arg);
        if (argE is List.Empty ||
            argE is not IPair) {
            return new Syntax(SyntaxList.FromParams(new Identifier(new Symbol("quote")),
                                                      arg),
                                stx.SrcLoc);
        } else if (argE is SyntaxList stxListArg) {
            if (Form.IsKeyword("unquote", arg)) { // (quasiquote (unquote x))
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
                if (Form.IsKeyword("unquote-splicing", stxListArg.ElementAt<Syntax>(0))) {
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
}
