namespace Jig.Expansion;

public interface IExpansionRule {
    ParsedForm Expand(Syntax syntax, ExpansionContext context);
}

public abstract class Transformer : IExpansionRule {
    

    public ParsedForm Expand(Syntax syntax, ExpansionContext context)
    {
        
        Scope macroExpansionScope = new Scope();
        Syntax.AddScope(syntax, macroExpansionScope);
        context.ExtendWithScope(macroExpansionScope);
        var output = this.Transform(syntax);
        Syntax.ToggleScope(output, macroExpansionScope);
        var result =  context.Expand(output);
        return result;
    }

    public abstract Syntax Transform(Syntax syntax);


}

public delegate ParsedForm ExpansionFunction(Syntax syntax, ExpansionContext context);

public class CoreSyntaxRule : IExpansionRule {

    public CoreSyntaxRule(ExpansionFunction expansionFunc) {
        ParseProcedure = expansionFunc;
    }

    public ParsedForm Expand(Syntax syntax, ExpansionContext context) => ParseProcedure(syntax, context); 

    public ExpansionFunction ParseProcedure { get;}
    
}