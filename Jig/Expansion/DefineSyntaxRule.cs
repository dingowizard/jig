namespace Jig.Expansion;

public partial class CoreParseRules {

    public static SemiParsedForm DefineSyntax(Syntax syntax, ExpansionContext context) {
        // Console.WriteLine("in define-syntax core parse rule");
        // // var subForms = ((SyntaxList)Syntax.E(syntax)).ToArray<Syntax>();
        // // int formLength = subForms.Length;
        // // if (subForms.Length is not 3) {
        // //     throw new Exception($"bad syntax in define-syntax @ {syntax.SrcLoc}: expected 3 sub-forms, got {formLength}");
        // // }
        // // ParsedVariable var =
        // //     subForms[1] as ParsedVariable ?? throw new Exception($"bad syntax in define-syntax @ {syntax.SrcLoc}: expected first sub-form to be a parsed variable. Got {subForms[1]}, a {subForms[1].GetType()}");
        // // // TODO: probably we should only need to check that the syntax is a parsed define syntax at this point
        // // return new ParsedDefineSyntax(subForms[0], var, subForms[2]);
        // return syntax as ParsedDefineSyntax ?? throw new Exception();

        var stxList = (SyntaxList.NonEmpty)Syntax.E(syntax);
        
        if (stxList.Rest is not SyntaxList.NonEmpty { First: Identifier vr } rt)
            throw new Exception($"malformed define: {Syntax.ToDatum(syntax).Print()} ");
        var bg = new Parameter(
            vr.Symbol,
            vr.ScopeSet,
            context.ScopeLevel,
            context.VarIndex++,
            vr.SrcLoc);
        context.AddBinding(vr, bg);
        var parsedKW = new ParsedVariable.TopLevel(vr, bg, vr.SrcLoc);
                    
        // Expand third subform
        if (rt.Rest is not SyntaxList.NonEmpty tail) {
            throw new Exception($"malformed define-syntax:expected a third subform");
        }
        Syntax transformerStx = tail.First;
        
        // TODO: I think we might need a ParsedKeyword type?

        ParsedLambda transformerLambdaExpr = context.Expander.Evaluator.Expander.Expand(transformerStx, context) as ParsedLambda ?? throw new Exception(); // TODO: actually this should use the phase 1 runtime
        // DefineSyntax(parsedKW, transformerLambdaExpr, context);
        var transformerProcedure =
            context.Expander.Evaluator.EvaluateTransformerExpression(transformerLambdaExpr, context);
        // TODO: ditto
        // TODO: should it use ParsedVar rather than id? for define as well?
        context.Expander.Owner.Keywords.Add(parsedKW.Identifier, transformerProcedure);

        return new SemiParsedDefineSyntax((Identifier)stxList.First, parsedKW, transformerStx, stxList, syntax.SrcLoc);

    }
    
}

public class SemiParsedDefineSyntax : SemiParsedDefinition {
    // NOTE: this is a misnomer. After first pass, there is nothing left to do
    // So this just exists to return something of type ParsedForm

    public SemiParsedDefineSyntax(Identifier defStx, ParsedVariable var, Syntax transformer, SyntaxList expr, SrcLoc? srcLoc = null) : base(expr, srcLoc) {
        Keyword = defStx;
        Var = var;
        Transformer = transformer;
    }
    public Syntax Transformer {get; set;}

    public ParsedVariable Var {get; set;}

    public Identifier Keyword {get; set;}

    public override ParsedForm SecondPass(ExpansionContext context) {
        return new ParsedDefineSyntax(Keyword, Var, Transformer);
    }
}

