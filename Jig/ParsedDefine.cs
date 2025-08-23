using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;

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

    public static bool TryParse(Syntax stx, MacroExpander expander, ExpansionEnvironment ee, [NotNullWhen(returnValue: true)] out ParsedDefine? defineExpr) {
        if (Syntax.E(stx) is not SyntaxList stxList) {
            defineExpr = null;
            return false;
        }
        if (!Form.IsKeyword("define", stx)) {
            defineExpr = null;
            return false;
        }
        Debug.Assert(stxList.Count<Syntax>() == 3 || stxList.Count<Syntax>() == 2); // TODO: this should be a syntax error
        Identifier id = stxList.ElementAt<Syntax>(1) as Identifier
                        ?? throw new Exception($"syntax error: malformed define: expected first argument to be an identifier. Got {stxList.ElementAt<Syntax>(1)}");
        // TODO: hm...
        var binding = new Binding(id.Symbol,  ee.ScopeLevel, ee.VarIndex++);
        id.Symbol.Binding = binding ;
        if (id.ScopeSet.Count != 0) {
            // not top level
            expander.AddBinding(id, id.Symbol.Binding);
            defineExpr = new ParsedDefine(stxList.ElementAt<Syntax>(0),
                new ParsedVariable.Lexical(id, binding, id.SrcLoc),
                expander.Expand(stxList.ElementAt<Syntax>(2), ee, definesAllowed: false),
                stx.SrcLoc);
            return true;
        }

        defineExpr = stxList.Count<Syntax>() == 3
            ? new ParsedDefine(stxList.ElementAt<Syntax>(0),
                new ParsedVariable.TopLevel(id, binding, id.SrcLoc),
                expander.Expand(stxList.ElementAt<Syntax>(2), ee, definesAllowed: false),
                stx.SrcLoc)
            : new ParsedDefine(stxList.ElementAt<Syntax>(0),
                new ParsedVariable.TopLevel(id, binding, id.SrcLoc), stx.SrcLoc);
            
        return true;
    }

}