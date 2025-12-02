namespace Jig.Expansion;

public partial class CoreParseRules
{
    public static SemiParsedForm ParseLibraryForm(Syntax syntax, ExpansionContext context) {
        
        var syntaxes = ((SyntaxList)Syntax.E(syntax)).ToArray<Syntax>();
        if (!ParsedLibraryName.TryParse(syntaxes[1], out var name)) {
            throw new Exception($"malformed library name {syntaxes[1].Print()}");
        }
        
        if (!ParsedExportForm.TryParse(syntaxes[2], out var exportForm)) {
            throw new Exception($"malformed export form {syntaxes[2].Print()}");
        }
        
        if (!ParsedImportForm.TryParse(syntaxes[3], out ParsedImportForm importForm)) {
            throw new Exception($"malformed import form {syntaxes[2].Print()}");
        }
        // At this point, we need to have a new ExpansionContext to expand/parse the library bodies in
        ExpansionContext newContext = ExpansionContext.FromImportForm(importForm);
        // if (!ParsedLibraryBody.TryParse(syntaxes.AsSpan(4), newContext, out ParsedLibraryBody body)) {
        //     // TODO: probably TryParse should throw more specific Exception.
        //     // or whatever we're going to do when we don't use exceptions anymore
        //     throw new Exception($"malformed library body");
        // }
        
        // return new ParsedLibrary(stxList[0], name, exportForm, importForm, body, syntax.SrcLoc);
        throw new NotImplementedException();

    }

}
public class SemiParsedLibraryForm : SemiParsedForm {
    public SemiParsedLibraryForm(SyntaxList syntaxList, SrcLoc? syntaxSrcLoc) : base(syntaxList, syntaxSrcLoc) {
    }
    
    public Identifier Keyword {get;}
    public override ParsedForm SecondPass(ExpansionContext context) {
        throw new NotImplementedException();



    }
}
