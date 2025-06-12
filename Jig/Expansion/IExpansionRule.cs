namespace Jig.Expansion;

public interface IExpansionRule {
    ParsedExpr Expand(Syntax syntax, ExpansionContext context);
}

public abstract class Transformer : IExpansionRule {
    

    public ParsedExpr Expand(Syntax syntax, ExpansionContext context)
    {
        
        Scope macroExpansionScope = new Scope();
        Syntax.AddScope(syntax, macroExpansionScope);
        context.ExtendWithScope(macroExpansionScope);
        var output = context.ApplyTransformer(this, syntax);
        Syntax.ToggleScope(output, macroExpansionScope);
        return context.Expand(output);
    }
    
    
}

public delegate ParsedExpr ExpansionFunction(Syntax syntax, ExpansionContext context);

public class CoreSyntaxRule : IExpansionRule {

    public CoreSyntaxRule(ExpansionFunction expansionFunc) {
        ParseProcedure = expansionFunc;
    }

    public ParsedExpr Expand(Syntax syntax, ExpansionContext context) => ParseProcedure(syntax, context); 

    public ExpansionFunction ParseProcedure { get;}
    
}