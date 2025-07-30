using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Runtime.CompilerServices;

namespace Jig.Expansion;

public class ExpansionContext {

    public ExpansionContext(
        IRuntime runtime,
        Expander expander,
        SyntaxEnvironment? env = null,
        Dictionary<Syntax.Identifier, Binding>? bindings = null,
        int scopeLevel = 0,
        int varIndex = 0,
        bool definesAllowed = true) {
        
        Runtime = runtime;
        Expander = expander;
        _syntaxEnvironment = env ?? SyntaxEnvironment.Default;
        _bindings = bindings ?? new Dictionary<Syntax.Identifier, Binding>(); // TODO: this seems like it should be part of the environment
        ScopeLevel = scopeLevel;
        VarIndex = varIndex;
        DefinesAllowed = definesAllowed;
        
        // TODO: _bindings, ScopeLevel and VarIndex should be collected into one Type whose job it is to keep track of the resolved bindings for the parsed program
        // maybe with TryResolve etc bundled into it.
        // The keywords may need to be resolved just like any other variable.
        
        // although the syntax environment contains only core forms and transformers
        // and this includes runtime bindings (only runtime?)
    }
    public Expander Expander {get;}

    public bool TryFindKeyword(Syntax.Identifier kw, [NotNullWhen(returnValue: true)] out IExpansionRule? expansionRule) {
        if (_syntaxEnvironment.TryFind(kw, out expansionRule)) {
            return true;
        }
        return false;
    }

    public void AddKeyword(Syntax.Identifier kw, IExpansionRule expansionRule) {
        _syntaxEnvironment.Add(kw, expansionRule);
        
    }

    public Syntax ApplyTransformer(Transformer transformer, Syntax syntax) {
        if (transformer is BuiltinTransformer builtin) {
            return builtin.Transform(syntax);
        }
        return Runtime.ApplyTransformer(transformer, syntax);
    }

    public Binding[] Bindings => _bindings.Values.ToArray();

    public void AddBinding(Syntax.Identifier id, Binding binding, [CallerMemberName] string name = "", [CallerFilePath] string path = "", [CallerLineNumber] int line = 0) {
        
        if (!_bindings.TryAdd(id, binding)) {
            foreach (var kvp in _bindings) {
                if (kvp.Key.Symbol.Name == id.Symbol.Name && kvp.Key.ScopeSet == id.ScopeSet) {
                    Console.Error.WriteLine($"\ttrying to add {id.Symbol} which has scopeset \n\t\t{string.Join(", ", id.ScopeSet)}");
                    Console.Error.WriteLine($"\tBut there is a key with the same name with scopeset \n\t\t{string.Join(", ", kvp.Key.ScopeSet)}");
                    Console.Error.WriteLine($"\tThey have the same hash code? {kvp.Key.GetHashCode() == id.GetHashCode()}");
                    Console.Error.WriteLine($"\tThey the same object? {ReferenceEquals(id, kvp.Key)}");

                }
            }

            throw new Exception();
        }
    }

    private Dictionary<Syntax.Identifier, Binding> _bindings {get;}

    IEnumerable<Syntax.Identifier> FindCandidateIdentifiers(Syntax.Identifier id) {
        IEnumerable<Syntax.Identifier> sameName = _bindings.Keys.Where(i => i.Symbol.Name == id.Symbol.Name);
        // if (id.Symbol.Name == "y") {
        //     Console.WriteLine($"The search id -- {id} -- has the following scope sets: {string.Join(',', id.ScopeSet)}");
        //     Console.WriteLine($"Found {sameName.Count()} bindings with same name.");
        //     foreach (var b in sameName) {
        //         Console.WriteLine($"\tscope sets: {string.Join(',', b.ScopeSet)}");
        //     }
        // }
        var result = sameName.Where(i => i.ScopeSet.IsSubsetOf(id.ScopeSet));
        return result;
    }

    internal bool TryResolve(Syntax.Identifier id, [NotNullWhen(returnValue: true)] out Binding? binding) {
        var candidates = FindCandidateIdentifiers(id);
        var identifiers = candidates as Syntax.Identifier[] ?? candidates.ToArray();
        // if (id.Symbol.Name == "y") {
        //     Console.WriteLine($"TryResolve: found {identifiers.Length} candidates for 'y' at {id.SrcLoc} in {this.GetHashCode()}");
        // }
        if (identifiers.Length == 0) {
            binding = null;
            return false;
        }
        #pragma warning disable CS8600
        Syntax.Identifier maxID = identifiers.MaxBy(i => i.ScopeSet.Count);// ?? throw new Exception("impossible");
        Debug.Assert(maxID is not null);
        #pragma warning restore CS8600
        CheckUnambiguous(maxID, identifiers);
        binding = _bindings[maxID];
        return true;
    }

    static void CheckUnambiguous(Syntax.Identifier maxID, IEnumerable<Syntax.Identifier> candidates) {
        // TODO: understand this better
        foreach (var candidate in candidates) {
            if (!candidate.ScopeSet.IsSubsetOf(maxID.ScopeSet)) {
                throw new Exception($"ambiguous : {maxID}");
            }
        }

    }

    public void DefineSyntax(Syntax.Identifier id, IExpansionRule rule) {
        _syntaxEnvironment.Add(id, rule);
    }

    public ParsedExpr Expand(Syntax stx) => Expander.Expand(stx, this);

    public bool DefinesAllowed {get;}
    public int VarIndex {get; set;} // TODO: this should be private, but clients are currently setting it. Bah.
    public int ScopeLevel {get;}
    
    internal IRuntime Runtime {get;}

    public void SyntaxError(string s) {
        throw new NotImplementedException();
    }
    public ExpansionContext ExtendWithScope(Scope scope) {
        // new context has ScopeLevel = this.ScopeLevel + 1
        // not sure if we need to have the Scope. Not doing anything with it yet

        return new ExpansionContext(
            this.Runtime,
            this.Expander,
            this._syntaxEnvironment.Extend(),
            this._bindings,
            this.ScopeLevel + 1,
            0,
            this.DefinesAllowed
            
        );
    }
    public ExpansionContext ExtendWithExpressionContext() {
        return new ExpansionContext(
            this.Runtime,
            this.Expander,
            this._syntaxEnvironment,
            this._bindings,
            this.ScopeLevel,
            this.VarIndex,
            false
        );
    }

    private SyntaxEnvironment _syntaxEnvironment {get;}
}