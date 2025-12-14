using Jig.Expansion;

namespace Jig;

public interface ILibrary {
    public IEnumerable<Binding> VariableExports { get; }
    
    public IEnumerable<(Symbol, IExpansionRule)> KeywordExports { get; }
}