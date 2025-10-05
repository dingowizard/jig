using Jig.Expansion;

namespace Jig;

public interface ILibrary
{
    public IEnumerable<Binding> VariableExports { get; }
    
    // TODO: this should probably pare * Identifiers * and IExpansionRules
    public IEnumerable<Tuple<Symbol, IExpansionRule>> KeywordExports { get; }
}