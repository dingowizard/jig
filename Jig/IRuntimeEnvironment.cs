namespace Jig;

public interface IRuntimeEnvironment {
    public Dictionary<Symbol, IRuntimeBinding> TopLevels { get; }
}