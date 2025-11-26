namespace Jig;
public class ParsedDefineSyntax : Definition {
    public ParsedDefineSyntax(Syntax keyword, ParsedVariable var, Syntax syntax, SrcLoc? srcLoc = null) :
        base(SyntaxList.FromParams(keyword, var, syntax), srcLoc) {
    }
}
