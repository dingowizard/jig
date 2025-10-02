namespace Jig;

public class ParsedDefine : ParsedForm {

    internal ParsedDefine(Syntax keyword, ParsedVariable id, ParsedForm val, SrcLoc? srcLoc = null)
        : base(SyntaxList.FromParams(keyword, id, val), srcLoc) {
        Variable = id;
        Value = val;
    }

    internal ParsedDefine(Syntax keyword, ParsedVariable id, SrcLoc? srcLoc = null)
        : base(SyntaxList.FromParams(keyword, id), srcLoc) {
        Variable = id;
        Value = null;
    }
    public ParsedVariable Variable {get;}
    public ParsedForm? Value {get;}


}