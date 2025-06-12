namespace Jig.Expansion;

public partial class CoreParseRules {
    public static ParsedExpr ParseQuoteForm(Syntax syntax, ExpansionContext context) {
        var stxList = ((SyntaxList)Syntax.E(syntax)).ToArray<Syntax>();
        if (stxList.Length != 2) throw new Exception($"malformed quote expression: {syntax} @ {syntax.SrcLoc}");
        return new ParsedLiteral(stxList[0], stxList[1], syntax.SrcLoc);
        
    }
    
}