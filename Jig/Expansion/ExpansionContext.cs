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
            _bindings.Add(p);
        }
        ScopeLevel = 0;
        VarIndex = i;
        DefinesAllowed = true;
        
    }

    private ExpansionContext(
        IRuntime runtime,
        Expander expander,
        SyntaxEnvironment env,
        System.Collections.Generic.List<Parameter> bindings,
        int scopeLevel = 0,
        int varIndex = 0,
        bool definesAllowed = true) {
        
        Runtime = runtime;
        Expander = expander;
        _syntaxEnvironment = env;
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


    public void AddBinding(Parameter parameter, [CallerMemberName] string name = "", [CallerFilePath] string path = "", [CallerLineNumber] int line = 0) {
        _bindings.Add(parameter);
    }

    // TODO: why is this a dictionary?
    private System.Collections.Generic.List<Parameter> _bindings {get;}


    internal bool TryResolve(Identifier id, [NotNullWhen(returnValue: true)] out Parameter? binding) {
        var candidates = _bindings
            .Where(i => i.Symbol.Name == id.Symbol.Name)
            .Where(i => i.ScopeSet.IsSubsetOf(id.ScopeSet))
            .ToArray();
        if (candidates.Length == 0) {
            binding = null;
            return false;
        }
        Parameter? maxId = candidates.MaxBy(i => i.ScopeSet.Count);
        Debug.Assert(maxId is not null);
        CheckUnambiguous(maxId, candidates);
        binding = maxId;
        return true;
    }

    static void CheckUnambiguous(Identifier maxId, IEnumerable<Identifier> candidates) {
        // TODO: understand this better
        foreach (var candidate in candidates) {
            if (!candidate.ScopeSet.IsSubsetOf(maxId.ScopeSet)) {
                throw new Exception($"ambiguous : {maxId}");
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