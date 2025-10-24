using Jig.Expansion;

namespace Jig;

public interface ILibrary {
    public IEnumerable<Binding> VariableExports { get; }
    
    // TODO: this should probably pair * Identifiers * and IExpansionRules
    public IEnumerable<(Symbol, IExpansionRule)> KeywordExports { get; }
}