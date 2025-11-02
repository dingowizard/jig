namespace Jig.Expansion;

public interface IExpansionRule {
    Syntax Expand(Syntax syntax, ExpansionContext context);
}

public delegate SemiParsedForm ExpansionFunction(Syntax syntax, ExpansionContext context);

public class CoreSyntaxRule : IExpansionRule {

    public CoreSyntaxRule(ExpansionFunction expansionFunc) {
        ParseProcedure = expansionFunc;
    }

    // NOTE: this will actually return a SemiParsedForm because of the signature of ParseProcedure
    // this is important because this is how FirstPass terminates -- macros keep on expanding until they
    // their output is recognized as a semiparsed form
    public Syntax Expand(Syntax syntax, ExpansionContext context) => ParseProcedure(syntax, context); 

    public ExpansionFunction ParseProcedure { get;}
    
}