namespace Jig.Expansion;

public partial class CoreParseRules {

    public static SemiParsedForm ParseLambdaForm(Syntax syntax, ExpansionContext context) {

        return new  SemiParsedLambda((SyntaxList)Syntax.E(syntax), syntax.SrcLoc);
    }

}

public class SemiParsedLambda : SemiParsedExpression {
    public SemiParsedLambda(SyntaxList stxList, SrcLoc? srcLoc) : base(stxList, srcLoc) {
        SubForms = stxList.ToArray<Syntax>();
        if (SubForms.Length < 3) {
            throw new Exception($"malformed lambda {stxList.Print()}");
        }

    }
    public Syntax[] SubForms {get;}

    public override ParsedForm Expand(ExpansionContext context) {
        var lambdaScope = new Scope();
        context = context.ExtendWithScope(lambdaScope); // this resets VarIndex to zero
        foreach (var x in SubForms.Skip(1)) {
            Syntax.AddScope(x, lambdaScope);
        }
        var ps = ParsedLambda.LambdaParameters.Parse(SubForms[1], context);
        
        // TODO: parameters and bodies need to be parsed before
        // call to ParsedLambda cstor below, because
        // otherwise VarIndex won't give the right number of scope variables. Yuck!
        // var bodies = subForms.Skip<Syntax>(2).Select(context.Expand).ToArray();
        ParsedForm[] bodies = context.ExpandSequence(SubForms.Skip<Syntax>(2));
        
        // TODO: ensure that bodies has at least one expression
        return new ParsedLambda(
            SubForms[0],
            ps,
            context.VarIndex,
            bodies,
            SrcLoc);
    }
}
