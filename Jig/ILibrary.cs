using Jig.Expansion;

namespace Jig;

public interface ILibrary
{
    public IEnumerable<IRuntimeBinding> VariableExports { get; }
    
    // TODO: this should probably pare * Identifiers * and IExpansionRules
    public IEnumerable<Tuple<Symbol, IExpansionRule>> KeywordExports { get; }
}