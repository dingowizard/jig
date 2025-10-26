using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Runtime.CompilerServices;

namespace Jig.Expansion;

public class ExpansionContext {

    public ExpansionContext(IEvaluator evaluator, IEnumerable<Parameter> topLevels) {
        Runtime = evaluator.Runtime;
        // TODO: seems like we can simplify the arguments to a bunch of these constructors once we get things running
        Expander = evaluator.Expander;
        _syntaxEnvironment = evaluator.Keywords;
        _bindings = new System.Collections.Generic.List<Parameter>(); // TODO: this seems like it should be part of the environment
        int i = 0;
        foreach (var p in topLevels) {
            // _bindings.Add(new Identifier(sym), new Parameter(sym, 0, i++, null));
            _bindings.Add(p); // TODO: why????
            
        }
        ScopeLevel = 0;
        VarIndex = i;
        DefinesAllowed = true;
        
    }

    private ExpansionContext(
        IRuntime runtime,
        Expander expander,
        SyntaxEnvironment env/* = null*/,
        System.Collections.Generic.List<Parameter> bindings,
        int scopeLevel = 0,
        int varIndex = 0,
        bool definesAllowed = true) {
        
        Runtime = runtime;
        Expander = expander;
        _syntaxEnvironment = env/* ?? SyntaxEnvironment.Default*/;
        _bindings = bindings; // TODO: this seems like it should be part of the environment
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


    public void AddBinding(Identifier id, Parameter parameter, [CallerMemberName] string name = "", [CallerFilePath] string path = "", [CallerLineNumber] int line = 0) {
        _bindings.Add(parameter);
        
        
        // if (!_bindings.TryAdd(id, parameter)) {
        //     foreach (var kvp in _bindings) {
        //         if (kvp.Key.Symbol.Name == id.Symbol.Name && kvp.Key.ScopeSet == id.ScopeSet) {
        //             Console.Error.WriteLine($"\ttrying to add {id.Symbol} which has scopeset \n\t\t{string.Join(", ", id.ScopeSet)}");
        //             Console.Error.WriteLine($"\tBut there is a key with the same name with scopeset \n\t\t{string.Join(", ", kvp.Key.ScopeSet)}");
        //             Console.Error.WriteLine($"\tThey have the same hash code? {kvp.Key.GetHashCode() == id.GetHashCode()}");
        //             Console.Error.WriteLine($"\tThey the same object? {ReferenceEquals(id, kvp.Key)}");
        //
        //         }
        //     }
        //
        //     throw new Exception();
        // }
    }

    // TODO: why is this a dictionary?
    private System.Collections.Generic.List<Parameter> _bindings {get;}

    IEnumerable<Parameter> FindCandidateIdentifiers(Identifier id) {
        IEnumerable<Parameter> sameName = _bindings.Where(i => i.Symbol.Name == id.Symbol.Name);
        // var name = "reverse";
        // if (id.Symbol.Name == name) {
        //     Console.WriteLine($"The search id -- {id} -- has the following scope sets: {string.Join(',', id.ScopeSet)}");
        //     Console.WriteLine($"Found {sameName.Count()} bindings with same name.");
        //     foreach (var b in sameName) {
        //         Console.WriteLine($"\tscope sets: {string.Join(',', b.ScopeSet)}");
        //         
        //     }
        //     Console.WriteLine("The bindings:");
        //     foreach (var (i, b) in _bindings) {
        //         Console.WriteLine($"\t{i}, {b}");
        //     }
        // }
        var result = sameName.Where(i => i.ScopeSet.IsSubsetOf(id.ScopeSet));
        return result;
    }

    internal bool TryResolve(Identifier id, [NotNullWhen(returnValue: true)] out Parameter? binding) {
        var candidates = FindCandidateIdentifiers(id);
        var identifiers = candidates as Parameter[] ?? candidates.ToArray();
        // var name = "reverse";
        // if (id.Symbol.Name == name) {
        //     Console.WriteLine($"TryResolve: found {identifiers.Length} candidates for '{name}' at {id.SrcLoc} in {this.GetHashCode()}");
        // }
        if (identifiers.Length == 0) {
            binding = null;
            return false;
        }
        #pragma warning disable CS8600
        Parameter maxID = identifiers.MaxBy(i => i.ScopeSet.Count);// ?? throw new Exception("impossible");
        Debug.Assert(maxID is not null);
        #pragma warning restore CS8600
        CheckUnambiguous(maxID, identifiers);
        binding = maxID;
        return true;
    }

    static void CheckUnambiguous(Identifier maxID, IEnumerable<Identifier> candidates) {
        // TODO: understand this better
        foreach (var candidate in candidates) {
            if (!candidate.ScopeSet.IsSubsetOf(maxID.ScopeSet)) {
                throw new Exception($"ambiguous : {maxID}");
            }
        }

    }


    public ParsedForm Expand(Syntax stx) => Expander.Expand(stx, this);

    public bool DefinesAllowed {get;}
    public int VarIndex {get; set;} // TODO: this should be private, but clients are currently setting it. Bah.
    public int ScopeLevel {get;}
    
    internal IRuntime Runtime {get;}

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
            true // TODO: I'm assuming defines are always allowed in a new scope
            
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

    public ParsedForm[] ExpandSequence(IEnumerable<Syntax> syntaxes) {
        return Expander.ExpandSequence(syntaxes, this).ToArray();
    }

}