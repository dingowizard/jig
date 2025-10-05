using Jig.Expansion;

namespace Jig;

public interface IRuntimeEnvironment {
    public Dictionary<Parameter, Binding> TopLevels { get; }
    
    public void DefineTopLevel (Parameter symbol, Binding binding);
    
}