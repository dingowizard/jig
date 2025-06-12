namespace Jig.Expansion;

public partial class CoreParseRules {

    public static ParsedExpr ParseLambdaForm(Syntax syntax, ExpansionContext context) {

        var subForms = ((SyntaxList)Syntax.E(syntax)).ToArray<Syntax>();
        var lambdaScope = new Scope();
        context = context.ExtendWithScope(lambdaScope); // this resets VarIndex to zero
        foreach (var x in subForms.Skip(1)) {
            Syntax.AddScope(x, lambdaScope);
        }
        var ps = ParsedLambda.LambdaParameters.Parse(subForms[1], context);

        // TODO: parameters and bodies need to be parsed before
        // call to ParsedLambda cstor below, because
        // otherwise VarIndex won't give the right number of scope variables. Yuck!
        var bodies = subForms.Skip<Syntax>(2).Select(context.Expand).ToArray();
        
        // TODO: ensure that bodies has at least one expression
        return new ParsedLambda(
            subForms[0],
            ps,
            context.VarIndex,
            bodies,
            syntax.SrcLoc);
    }

}
