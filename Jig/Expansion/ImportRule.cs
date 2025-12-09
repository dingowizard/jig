namespace Jig.Expansion;

public partial class CoreParseRules {

    public static SemiParsedImportForm ParseImportForm(Syntax syntax, ExpansionContext context) {
        // TODO: a lot of this logic is repeated elsewhere.
        // see VM/Library.cs and ParsedImportForm.TryParse and ParsedLibrary
        // we should decide where it actually lives
        if (Syntax.E(syntax) is not SyntaxList.NonEmpty list) {
            throw new Exception();
        }
        if (list.First is not Identifier kw) {
            throw new Exception("malformed library form: expected 'import'");
        }
        if (kw.Symbol.Name != "import") {
            throw new Exception("malformed library form: expected 'import'");
        }
        List more = list.Rest;
        System.Collections.Generic.List<ParsedImportSpec> importSpecs = [];
        while (more is SyntaxList.NonEmpty rest) {
            Syntax next = rest.First;
            if (ParsedImportSpec.TryParse(next, out ParsedImportSpec spec)) {
                importSpecs.Add(spec);
                more = rest.Rest;
                continue;
            }
            throw new Exception("in import form, expected an import spec, but got {next.Print()}");
        }
        
        // during the first pass, an import form needs to get the keywords out of the libraries
        // so that they can be used in the body forms beneath the import form
        
        System.Collections.Generic.List<ILibrary> importedLibraries = [];
        foreach (var importSpec in importSpecs) {
            if (LibraryLibrary.Instance.TryFindLibrary(importSpec, out ILibrary? library)) {
                importedLibraries.Add(library);
            } else {
                throw new Exception($"could not find library from spec: {importSpec.Print()}");
            }
        }
        return new SemiParsedImportForm(importedLibraries.ToArray(), kw, importSpecs.ToArray(), syntax.SrcLoc);
    }
    
}

public class SemiParsedImportForm : SemiParsedForm {
    public SemiParsedImportForm(ILibrary[] libraries, Identifier kw, ParsedImportSpec[] specs,
        SrcLoc? srcLoc = null)
        : base(SyntaxList.FromIEnumerable([kw, ..specs]), srcLoc) {
        Keyword = kw;
        Libraries = libraries;
        ImportSpecs = specs;
    }
    
    public Identifier Keyword { get; }

    public ParsedImportSpec[] ImportSpecs { get; }

    public ILibrary[] Libraries { get; }

    public override ParsedForm SecondPass(ExpansionContext context) {
        for (int i = 0; i < Libraries.Length; i++) {
            context.Expander.Owner.ImportVariables(Libraries[i], ImportSpecs[i].Level);
        }

        return new ParsedImportForm(Keyword, ImportSpecs, SrcLoc);
    }
}