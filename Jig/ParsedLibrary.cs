using System.Collections;

namespace Jig;

public class ParsedLibrary : ParsedForm {

    internal ParsedLibrary(
        Syntax keyword,
        ParsedLibraryName name,
        ParsedExportForm exports,
        ParsedImportForm imports,
        ParsedLibraryBody body,
        SrcLoc? srcLoc = null)
        : base(SyntaxList.FromParams(keyword, exports, imports).Concat<Syntax>(body).ToSyntaxList(), srcLoc)
    {
    }

}

public class ParsedLibraryName : ParsedForm
{
    // TODO: rename to ParsedLibraryReference?
    internal ParsedLibraryName(IEnumerable<Identifier> names, ParsedLibraryVersion version, SrcLoc? srcLoc = null)
        : base(SyntaxList
            .FromIEnumerable(names)
            .ToList<Syntax>()
            .Append<Syntax>(version)
            .ToSyntaxList(),
            srcLoc
        ) {}
    
}

public class ParsedLibraryVersion : ParsedForm {

    internal ParsedLibraryVersion(IEnumerable<Literal> literals, SrcLoc? srcLoc = null) :
        base(SyntaxList.FromIEnumerable(literals), srcLoc)
    {
        
    }
    
}

public class ParsedExportForm : ParsedForm
{
    // TODO: should be sequence of ExportSpecs not vars
    // TODO: vars are Identifiers or ParsedTopVars? or parameters?
    internal ParsedExportForm(Identifier kw, IEnumerable<Identifier> vars, SrcLoc? srcLoc = null) :
        base(SyntaxList.FromParams(kw).Concat<Syntax>(vars).ToSyntaxList(), srcLoc) {}
}

public class ParsedImportForm : ParsedForm
{
    internal ParsedImportForm(Identifier kw, IEnumerable<ParsedLibraryName> libs, SrcLoc? srcLoc = null) :
        base(SyntaxList.FromParams(kw).Concat<Syntax>(libs).ToSyntaxList(), srcLoc)
    {
        
    } 
}

public class ParsedLibraryBody : IEnumerable<ParsedForm>
{
    // TODO: r6rs mandates that a library body be any number of defs followed by any number of exprs
    internal ParsedLibraryBody(IEnumerable<ParsedForm> forms, SrcLoc? srcLoc = null) {}
    public IEnumerator<ParsedForm> GetEnumerator()
    {
        throw new NotImplementedException();
    }

    IEnumerator IEnumerable.GetEnumerator()
    {
        return GetEnumerator();
    }
}