namespace Jig;

public interface IRuntimeEnvironment {
    public Dictionary<Symbol, IRuntimeBinding> TopLevels { get; }
    
    public void DefineTopLevel (Symbol symbol, IRuntimeBinding runtimeBinding);
    
}