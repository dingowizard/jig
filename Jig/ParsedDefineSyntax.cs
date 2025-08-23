namespace Jig;
public class ParsedDefineSyntax : ParsedForm {
    public ParsedDefineSyntax(Syntax keyword, ParsedVariable var, Syntax syntax, SrcLoc? srcLoc = null) :
        base(SyntaxList.FromParams(keyword, var, syntax), srcLoc) {
        throw new NotImplementedException();
    }
}
