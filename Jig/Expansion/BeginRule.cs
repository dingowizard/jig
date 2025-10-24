namespace Jig.Expansion;

public partial class CoreParseRules
{
    // TODO: should these all return the more specific type (here, ParsedBegin)?
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
            context.ExpandSequence(stxList.Skip<Syntax>(1)),
            syntax.SrcLoc);
    }

}