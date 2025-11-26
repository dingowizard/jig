using Jig.Expansion;
namespace Jig;

public class ParsedDefine : Definition {

    internal ParsedDefine(Syntax keyword, Parameter id, ParsedForm val, SrcLoc? srcLoc = null)
        : base(SyntaxList.FromParams(keyword, id, val), srcLoc) {
        Variable = id;
        Value = val;
    }

    internal ParsedDefine(Syntax keyword, Parameter id, SrcLoc? srcLoc = null)
        : base(SyntaxList.FromParams(keyword, id), srcLoc) {
        Variable = id;
        Value = null;
    }
    public Parameter Variable {get;}
    public ParsedForm? Value {get;}


}