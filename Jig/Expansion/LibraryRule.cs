namespace Jig.Expansion;

public partial class CoreParseRules
{
    public static SemiParsedForm ParseLibraryForm(Syntax syntax, ExpansionContext context)
    {
        // NOTE: this code is run in the second pass of the expander
        // this should be fine because library forms are only allowed as the single toplevel
        // form in their own file
        // Or at the repl, in which case it is not being eval-ed as part of a sequence
        // (where forward references would be an issue)
        // var stxList = ((SyntaxList)Syntax.E(syntax)).ToArray<Syntax>();
        // if (!ParsedLibraryName.TryParse(stxList[1], out var name)) {
        //     throw new Exception($"malformed library name {stxList[1].Print()}");
        // }
        //
        // if (!ParsedExportForm.TryParse(stxList[2], out var exportForm)) {
        //     throw new Exception($"malformed export form {stxList[2].Print()}");
        // }
        //
        // if (!ParsedImportForm.TryParse(stxList[3], out ParsedImportForm importForm)) {
        //     throw new Exception($"malformed import form {stxList[2].Print()}");
        // }
        // // At this point, we need to have a new ExpansionContext to expand/parse the library bodies in
        // ExpansionContext newContext = ExpansionContext.FromImportForm(context, importForm);
        // if (!ParsedLibraryBody.TryParse(stxList.AsSpan(4), newContext, out ParsedLibraryBody body)) {
        //     // TODO: probably TryParse should throw more specific Exception.
        //     // or whatever we're going to do when we don't use exceptions anymore
        //     throw new Exception($"malformed library body");
        // }
        //
        // return new ParsedLibrary(stxList[0], name, exportForm, importForm, body, syntax.SrcLoc);
        throw new NotImplementedException();

    }

}