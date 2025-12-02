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
        if (!ParsedLibraryBody.TryParse(syntaxes.AsSpan(4), context, out ParsedLibraryBody body)) {
            // TODO: probably TryParse should throw more specific Exception.
            // or whatever we're going to do when we don't use exceptions anymore
            throw new Exception($"malformed library body");
        }
        
        return new SemiParsedLibraryForm((Identifier)syntaxes[0], name, exportForm,  importForm, body, syntax.SrcLoc);
        
        // return new ParsedLibrary(stxList[0], name, exportForm, importForm, body, syntax.SrcLoc);
        throw new NotImplementedException();

    }

}
public class SemiParsedLibraryForm : SemiParsedForm {
    public SemiParsedLibraryForm(
        Identifier keyword,
        ParsedLibraryName parsedLibraryName,
        ParsedExportForm exportForm,
        ParsedImportForm parsedImportForm,
        ParsedLibraryBody parsedLibraryBody, SrcLoc? srcLoc)
        : base(SyntaxList.FromParams(keyword, parsedLibraryName, exportForm, parsedImportForm).Concat<Syntax>(parsedLibraryBody).ToSyntaxList(), srcLoc)
    {
        Keyword = keyword;
        LibraryName = parsedLibraryName;
        ExportForm = exportForm;
        ImportForm = parsedImportForm;
        Body = parsedLibraryBody;
    }
    public ParsedLibraryBody Body {get;}
    public ParsedImportForm ImportForm {get;}
    public ParsedExportForm ExportForm {get;}
    public ParsedLibraryName LibraryName {get;}

    public Identifier Keyword {get;}
    public override ParsedForm SecondPass(ExpansionContext context) {
        // Do the second pass expansion of the library bodies
        ParsedLibrary result = new ParsedLibrary(Keyword, LibraryName, ExportForm, ImportForm, Body);
        LibraryLibrary.Instance.RegisterLibrary(result);
        return result;
    }
    
}
