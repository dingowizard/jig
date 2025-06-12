namespace Jig.Expansion;

public partial class CoreParseRules
{
    public static ParsedExpr ParseBeginForm(Syntax syntax, ExpansionContext context) {
        var stxList = ((SyntaxList)Syntax.E(syntax)).ToArray<Syntax>();
        if (!context.DefinesAllowed) {
            if (stxList.Length < 2) {
                throw new Exception($"invalid syntax in begin @ {syntax.SrcLoc}: expected at least one expression");
            } 
        }
        return new ParsedBegin(
            stxList[0],
            stxList.Skip<Syntax>(1).Select(context.Expand).ToArray(),
            syntax.SrcLoc);
    }

}