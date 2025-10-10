namespace Jig.Expansion;

public partial class CoreParseRules
{
    public static ParsedForm ParseBeginForm(Syntax syntax, ExpansionContext context) {
        var stxList = ((SyntaxList)Syntax.E(syntax)).ToArray<Syntax>();
        if (!context.DefinesAllowed) {
            if (stxList.Length < 2) {
                throw new Exception($"invalid syntax in begin @ {syntax.SrcLoc}: expected at least one expression");
            } 
        }
        return new ParsedBegin(
            stxList[0],
            // TODO: make a ExpandSequence to be used by various
            context.ExpandLambdaBody(stxList.Skip<Syntax>(1)),
            syntax.SrcLoc);
    }

}