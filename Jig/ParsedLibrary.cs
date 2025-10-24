using System.Collections;
using Jig.Expansion;

namespace Jig;

public class ParsedLibrary : ParsedForm {
    public ParsedLibraryName Name { get; }
    public ParsedExportForm Exports { get; }
    public ParsedImportForm Imports { get; }
    public ParsedLibraryBody Body { get; }

    internal ParsedLibrary(
        Syntax keyword,
        ParsedLibraryName name,
        ParsedExportForm exports,
        ParsedImportForm imports,
        ParsedLibraryBody body,
        SrcLoc? srcLoc = null)
        : base(SyntaxList.FromParams(keyword, name, exports, imports).Concat<Syntax>(body).ToSyntaxList(), srcLoc)
    {
        Name = name;
        Exports = exports;
        Imports = imports;
        Body = body;
    }

}

public class ParsedLibraryName : ParsedForm
{
    public IEnumerable<Identifier> Names { get; }
    public ParsedLibraryVersion Version { get; }

    // TODO: rename to ParsedLibraryReference?
    internal ParsedLibraryName(IEnumerable<Identifier> names, ParsedLibraryVersion version, SrcLoc? srcLoc = null)
        : base(SyntaxList
            .FromIEnumerable(names)
            .ToList<Syntax>()
            .Append<Syntax>(version)
            .ToSyntaxList(),
            srcLoc
        )
    {
        Names = names;
        Version = version;
    }

    public static bool TryParse(Syntax stx, out ParsedLibraryName parsedLibraryName) {
        throw new NotImplementedException();
    }
}

public class ParsedLibraryVersion : ParsedForm {
    public IEnumerable<Literal> Literals { get; }

    internal ParsedLibraryVersion(IEnumerable<Literal> literals, SrcLoc? srcLoc = null) :
        base(SyntaxList.FromIEnumerable(literals), srcLoc)
    {
        Literals = literals;
    }
    
}

public class ParsedExportForm : ParsedForm
{
    public IEnumerable<Identifier> Vars { get; }

    // TODO: should be sequence of ExportSpecs not vars
    // TODO: vars are Identifiers or ParsedTopVars? or parameters?
    internal ParsedExportForm(Identifier kw, IEnumerable<Identifier> vars, SrcLoc? srcLoc = null) :
        base(SyntaxList.FromParams(kw).Concat<Syntax>(vars).ToSyntaxList(), srcLoc)
    {
        Vars = vars;
    }

    public static bool TryParse(Syntax stx, out ParsedExportForm exportForm) {
        throw new NotImplementedException();
    }
}

public class ParsedImportForm : ParsedForm
{
    public IEnumerable<ParsedLibraryName> Libs { get; }

    internal ParsedImportForm(Identifier kw, IEnumerable<ParsedLibraryName> libs, SrcLoc? srcLoc = null) :
        base(SyntaxList.FromParams(kw).Concat<Syntax>(libs).ToSyntaxList(), srcLoc)
    {
        Libs = libs;
    }

    public static bool TryParse(Syntax stx, out ParsedImportForm parsedImportForm)
    {
        throw new NotImplementedException();
    }
}

public class ParsedLibraryBody : IEnumerable<ParsedForm>
{
    public IEnumerable<ParsedForm> Forms { get; }

    // TODO: r6rs mandates that a library body be any number of defs followed by any number of exprs
    internal ParsedLibraryBody(IEnumerable<ParsedForm> forms, SrcLoc? srcLoc = null)
    {
        Forms = forms;
    }
    public IEnumerator<ParsedForm> GetEnumerator()
    {
        foreach (var form in Forms) {
            yield return form;
        }
    }

    IEnumerator IEnumerable.GetEnumerator()
    {
        return GetEnumerator();
    }

    public static bool TryParse(Span<Syntax> stxes, ExpansionContext context, out ParsedLibraryBody parsedLibraryBody) {
        throw new NotImplementedException();
    }
    
}