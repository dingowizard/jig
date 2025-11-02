namespace Jig.Expansion;

public abstract class Transformer : IExpansionRule {
    

    public Syntax Expand(Syntax syntax, ExpansionContext context)
    {
        
        Scope macroExpansionScope = new Scope();
        Syntax.AddScope(syntax, macroExpansionScope);
        context.ExtendWithScope(macroExpansionScope);
        var output = this.Transform(syntax);
        Syntax.ToggleScope(output, macroExpansionScope);
        return output;
    }

    public abstract Syntax Transform(Syntax syntax);


}