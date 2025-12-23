using System.Collections;
using Jig.Expansion;

namespace Jig;

public class ParsedLibrary : ParsedForm {
    public ParsedLibraryName Name { get; }
    public ParsedExportForm Exports { get; }
    public ParsedImportForm ImportForm { get; }
    public ParsedLibraryBody Body { get; }

    internal ParsedLibrary(
        Syntax keyword,
        ParsedLibraryName name,
        ParsedExportForm exports,
        ParsedImportForm importForm,
        ParsedLibraryBody body,
        SrcLoc? srcLoc = null)
        : base(SyntaxList.FromParams(keyword, name, exports, importForm).Concat<Syntax>(body).ToSyntaxList(), srcLoc)
    {
        Name = name;
        Exports = exports;
        ImportForm = importForm;
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
        // library name is a list with one or more identifiers followed by an optional library version

        if (Syntax.E(stx) is not SyntaxList.NonEmpty list) {
            throw new Exception();
        }
        Syntax first =  list.First;
        if (first is not Identifier id) {
            throw new  Exception($"malformed library name {stx.Print()}");
        }
        SyntaxList rest = list.Rest;
        System.Collections.Generic.List<Identifier> ids = [];
        ids.Add(id);
        while (rest is SyntaxList.NonEmpty more) {
            // TODO: barf
            Identifier? name = more.First as Identifier;
            if (name is null) throw new NotImplementedException();
            ids.Add(name);
            rest = more.Rest;
            
        }
        // TODO: handle situations where there is a version number
        // TODO: catch some more errors
        parsedLibraryName = new ParsedLibraryName(ids.ToArray(), new ParsedLibraryVersion([]));
        return true;
        
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
        if (Syntax.E(stx) is not SyntaxList.NonEmpty list) {
            throw new Exception();
        }
        Identifier? kw =  list.First as Identifier;
        if (kw is null) {
            throw new Exception("malformed libary form: expected 'export'");
        }
        if (kw.Symbol.Name != "export") {
            throw new Exception("malformed libary form: expected 'export'");
        }
        List more = list.Rest;
        System.Collections.Generic.List<Identifier> vars = [];
        while (more is SyntaxList.NonEmpty rest) {
            if (rest.First is Identifier var) {
                vars.Add(var);
                more = rest.Rest;
                continue;
            } else {
                throw new Exception($"export doesn't know what to do with {rest.Print()}");

            }
        }
        exportForm = new ParsedExportForm(kw, vars);
        return true;
    }
}

public class ParsedImportSpec : ParsedForm {
    
    // NOTE: import spec is needed by LibraryLibrary to find the library
    // then it's needed by import to put bindings into right level, rename them, etc

    public ParsedImportSpec(IEnumerable<Identifier> ids, int level = 0, SrcLoc? srcLoc = null) : base(ids.ToSyntaxList(), srcLoc) {
        Name = ids.Select(id => id.Symbol).ToArray();
        Level = level;
    }
    
    public Symbol[] Name { get; } // TODO: not sure whether to prefer Symbols or identifiers.
    public int Level {get; set;}
    // In ParsedLibraryName it was ids for whatever.
    // TODO: meta level
    // TODO: version constraints
    // TODO: subset of exports to import
    // TODO: renames
    public static bool TryParse(Syntax stx, out ParsedImportSpec parsedImportSpec) {
        // TODO: handle situations where import spec is more than an library name
        if (Syntax.E(stx) is not SyntaxList.NonEmpty list) {
            throw new Exception($"stx was {stx.Print()}");
        }
        Syntax first =  list.First;
        // an import set might start with for
        // in which case it has the form
        // (for <import set> <import level> ...)

        if (first is Identifier {Symbol.Name: "for"}) {
            if (list.Rest is not SyntaxList.NonEmpty forMore) {
                throw new  Exception($"malformed library name {stx.Print()}");
            }
            if (!ParsedImportSet.TryParse(forMore.First, out var importSet)) {
                throw new  Exception($"malformed library name {stx.Print()}");
            }
            SyntaxList.NonEmpty importLevels = forMore.Rest as  SyntaxList.NonEmpty ?? throw new Exception("malformed import list");
            
            // for now we'll just support one import-level and it will either be 'run or 'expand
            int level = 0;
            if (importLevels.First is Identifier {Symbol.Name: "run"}) {
                level = 0;
            } else if (importLevels.First is Identifier {Symbol.Name: "expand"}) {
                level = 1;
            } else {
                throw new  Exception($"malformed import level {importLevels.Print()}");
            }
            parsedImportSpec = new ParsedImportSpec(importSet.Names, level);
            return true;
        }

        if (ParsedImportSet.TryParse(stx, out ParsedImportSet parsedImportSet)) {
            parsedImportSpec = new ParsedImportSpec(parsedImportSet.Names);
            return true;
        } else {
            throw  new  Exception($"malformed import spec {stx.Print()}");
            
        }
        
        System.Collections.Generic.List<Identifier> ids = [];
        if (first is Identifier i) {
            ids.Add(i);
        } else {
            throw new  Exception($"malformed library name {stx.Print()}");
        }
        SyntaxList rest = list.Rest;
        while (first is Identifier id && rest is SyntaxList.NonEmpty more) {
            ids.Add(id);
            rest = more.Rest;
            first = more.First;
            
        }
        // TODO: handle situations where there is a version number
        // TODO: catch some more errors
        parsedImportSpec = new ParsedImportSpec(ids.ToArray());
        return true;
    }
}

public class ParsedImportSet {

    public ParsedImportSet(IEnumerable<Identifier> names) {
        Names = names;
    }
    public static bool TryParse(Syntax stx, out ParsedImportSet importSet) {
        if (Syntax.E(stx) is not SyntaxList.NonEmpty list) {
            throw new Exception($"stx was {stx.Print()}");
        }
        
        System.Collections.Generic.List<Identifier> ids = [];
        // if (first is Identifier i) {
        //     ids.Add(i);
        // } else {
        //     throw new  Exception($"malformed library name {stx.Print()}");
        // }
        SyntaxList rest = list;
        while (rest is SyntaxList.NonEmpty more) {
            if (more.First is not Identifier id) {
                throw new  Exception($"malformed import set {stx.Print()}");
            }
            ids.Add(id);
            rest = more.Rest;
            
        }
        // TODO: handle situations where there is a version number
        // TODO: catch some more errors
        // Console.WriteLine($"making import set with {string.Join(",", ids)}");
        importSet = new ParsedImportSet(ids.ToArray());
        return true;
    }
    public IEnumerable<Identifier> Names {get;}
}

public class ParsedImportForm : ParsedForm
{
    public IEnumerable<ParsedImportSpec> Specs { get; }

    internal ParsedImportForm(Identifier kw, IEnumerable<ParsedImportSpec> specs, SrcLoc? srcLoc = null) :
        base(SyntaxList.FromParams(kw).Concat<Syntax>(specs).ToSyntaxList(), srcLoc)
    {
        Specs = specs;
    }

    public static bool TryParse(Syntax stx, out ParsedImportForm parsedImportForm) {
        if (Syntax.E(stx) is not SyntaxList.NonEmpty list) {
            throw new Exception();
        }
        Identifier? kw =  list.First as Identifier;
        if (kw is null) {
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
        parsedImportForm = new ParsedImportForm(kw, importSpecs);
        return true;
    }
}

public class ParsedLibraryBody : IEnumerable<Syntax> {

    // TODO: r6rs mandates that a library body be any number of defs followed by any number of exprs
    // TODO: this class doesn't do anything
    internal ParsedLibraryBody(SyntaxList list, SrcLoc? srcLoc = null) {
        Syntaxes = list;
    }
    public SyntaxList Syntaxes {get;}
    public IEnumerator<Syntax> GetEnumerator() {
        foreach (var stx in Syntaxes) {
            yield return (Syntax)stx;
        }
    }

    IEnumerator IEnumerable.GetEnumerator() {
        return GetEnumerator();
    }

    public static bool TryParse(SyntaxList stxes, ExpansionContext context, out ParsedLibraryBody parsedLibraryBody) {
        parsedLibraryBody = new ParsedLibraryBody(stxes);
        return true;
    }
    
}