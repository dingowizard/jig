namespace Jig.Expansion;

public partial class CoreParseRules {

    public static ParsedExpr ParseIfForm(Syntax syntax, ExpansionContext context) {

        var subForms = ((SyntaxList)Syntax.E(syntax)).ToArray<Syntax>();
        System.Collections.Generic.List<ParsedExpr> xs = [];
        context = context.ExtendWithExpressionContext();
        // TODO: a definition wouldn't be allowed for any of the subforms,
        // but a begin with definitions would be allowed, just not a splicing begin
        // and it would have to have at least one expression and end with an expression
        foreach (var x in subForms.Skip<Syntax>(1)) {
            ParsedExpr bodyExpr = context.Expand(x);
            xs.Add(bodyExpr);
        }
        if (xs.Count == 2) {
            return new ParsedIf(subForms[0], xs.ElementAt(0), xs.ElementAt(1), syntax.SrcLoc);
        }
        if (xs.Count == 3) {
            return new ParsedIf(subForms[0], xs.ElementAt(0), xs.ElementAt(1), xs.ElementAt(2), syntax.SrcLoc);
        }
        throw new Exception($"syntax error: malformed 'if' @{syntax.SrcLoc}: {Syntax.E(syntax)}");
        
    }

}
