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

public class ParsedLibraryName : ParsedForm {
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

public class ParsedImportSpec : ParsedForm {
    
    // NOTE: import spec is needed by LibraryLibrary to find the library
    // then it's needed by import to put bindings into right level, rename them, etc

    public ParsedImportSpec(ISchemeValue x, SrcLoc? srcLoc = null) : base(x, srcLoc) {}
    
    public Symbol[] Name { get; } // TODO: not sure whether to prefer Symbols or identifiers.
                                  // In ParsedLibraryName it was ids for whatever.
    // TODO: meta level
    // TODO: version constraints
    // TODO: subset of exports to import
    // TODO: renames
}

public class ParsedImportForm : ParsedForm
{
    public IEnumerable<ParsedImportSpec> Libs { get; }

    internal ParsedImportForm(Identifier kw, IEnumerable<ParsedImportSpec> libs, SrcLoc? srcLoc = null) :
        base(SyntaxList.FromParams(kw).Concat<Syntax>(libs).ToSyntaxList(), srcLoc)
    {
        Libs = libs;
    }

    public static bool TryParse(Syntax stx, out ParsedImportForm parsedImportForm)
    {
        throw new NotImplementedException();
    }
}

public class ParsedLibraryBody : IEnumerable<ParsedForm> {

    // TODO: r6rs mandates that a library body be any number of defs followed by any number of exprs
    internal ParsedLibraryBody(IEnumerable<Definition> defns, IEnumerable<Expression> exprs, SrcLoc? srcLoc = null) {
        Definitions = defns;
        Expressions = exprs;
    }
    public IEnumerable<Expression> Expressions {get; }
    public IEnumerable<Definition> Definitions {get; }
    public IEnumerator<ParsedForm> GetEnumerator() {
        foreach (var definition in Definitions) {
            yield return definition;
        }
        foreach (var expr in Expressions) {
            yield return expr;
        }
    }

    IEnumerator IEnumerable.GetEnumerator() {
        return GetEnumerator();
    }

    public static bool TryParse(Span<Syntax> stxes, ExpansionContext context, out ParsedLibraryBody parsedLibraryBody) {
        throw new NotImplementedException();
    }
    
}