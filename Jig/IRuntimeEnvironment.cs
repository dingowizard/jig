namespace Jig;

public interface IRuntimeEnvironment {
    public Dictionary<Form.Symbol, IRuntimeBinding> TopLevels { get; }
}