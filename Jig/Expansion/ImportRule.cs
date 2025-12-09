namespace Jig.Expansion;

public partial class CoreParseRules {

    public static SemiParsedImportForm ParseImportForm(Syntax syntax, ExpansionContext context) {
        throw new NotImplementedException();
    }
    
}

public class SemiParsedImportForm : SemiParsedForm {
    public SemiParsedImportForm(ISchemeValue expr, SrcLoc? srcLoc = null) : base(expr, srcLoc) {}
    public override ParsedForm SecondPass(ExpansionContext context) {
        throw new NotImplementedException();
        // return new ParsedImportForm();
    }
}