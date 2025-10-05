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
        _bindings = new Dictionary<Identifier, Parameter>(); // TODO: this seems like it should be part of the environment
        int i = 0;
        foreach (var p in topLevels) {
            // TODO: topLevels should probably be identifiers, most likely parameters
            // _bindings.Add(new Identifier(sym), new Parameter(sym, 0, i++, null));
            _bindings.Add(p, p); // TODO: why????
            
        }
        ScopeLevel = 0;
        VarIndex = i;
        DefinesAllowed = true;
        
    }

    private ExpansionContext(
        IRuntime runtime,
        Expander expander,
        SyntaxEnvironment? env = null,
        Dictionary<Identifier, Parameter>? bindings = null,
        int scopeLevel = 0,
        int varIndex = 0,
        bool definesAllowed = true) {
        
        Runtime = runtime;
        Expander = expander;
        _syntaxEnvironment = env ?? SyntaxEnvironment.Default;
        _bindings = bindings ?? new Dictionary<Identifier, Parameter>(); // TODO: this seems like it should be part of the environment
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

    public bool TryFindKeyword(Identifier kw, [NotNullWhen(returnValue: true)] out IExpansionRule? expansionRule) {
        if (_syntaxEnvironment.TryFind(kw, out expansionRule)) {
            return true;
        }
        return false;
    }

    public void AddKeyword(Identifier kw, IExpansionRule expansionRule) {
        _syntaxEnvironment.Add(kw, expansionRule);
        
    }

    // public Syntax ApplyTransformer(Transformer transformer, Syntax syntax) {
    //     if (transformer is BuiltinTransformer builtin) {
    //         return builtin.Transform(syntax);
    //     }
    //     return Expander.Evaluator.Runtime.ApplyTransformer(transformer, syntax);
    // }

    public Parameter[] Bindings => _bindings.Values.ToArray();

    public void AddBinding(Identifier id, Parameter parameter, [CallerMemberName] string name = "", [CallerFilePath] string path = "", [CallerLineNumber] int line = 0) {
        
        if (!_bindings.TryAdd(id, parameter)) {
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

    public IEnumerable<Identifier> Bound => _bindings.Keys;

    private Dictionary<Identifier, Parameter> _bindings {get;}

    IEnumerable<Identifier> FindCandidateIdentifiers(Identifier id) {
        IEnumerable<Identifier> sameName = _bindings.Keys.Where(i => i.Symbol.Name == id.Symbol.Name);
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
        var identifiers = candidates as Identifier[] ?? candidates.ToArray();
        // var name = "reverse";
        // if (id.Symbol.Name == name) {
        //     Console.WriteLine($"TryResolve: found {identifiers.Length} candidates for '{name}' at {id.SrcLoc} in {this.GetHashCode()}");
        // }
        if (identifiers.Length == 0) {
            binding = null;
            return false;
        }
        #pragma warning disable CS8600
        Identifier maxID = identifiers.MaxBy(i => i.ScopeSet.Count);// ?? throw new Exception("impossible");
        Debug.Assert(maxID is not null);
        #pragma warning restore CS8600
        CheckUnambiguous(maxID, identifiers);
        binding = _bindings[maxID];
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

    public void DefineSyntax(ParsedVariable kw, SyntaxList stxs) {
        // _syntaxEnvironment.Add(id, rule);
        if (stxs is not SyntaxList.NonEmpty stxList) {
            throw new Exception($"malformed define-syntax:expected a third subform");
        }
        Syntax transformerStx =
            stxList.First;
        
        // TODO: I think we might need a ParsedKeyword type?

        // Expand third subform
        ParsedLambda transformerLambdaExpr = this.Expand(transformerStx) as ParsedLambda ?? throw new Exception(); // TODO: actually this should use the phase 1 runtime
        var transformerProcedure = Expander.Evaluator.EvaluateTransformerExpression(transformerLambdaExpr, this); // TODO: ditto
        // TODO: should it use ParsedVar rather than id? for define as well?
        Expander.Owner.Keywords.Add(kw.Identifier, transformerProcedure);
        Console.WriteLine($"added {kw.Identifier.Symbol.Print()} to phase {Expander.Owner.Phase} evaluator's keywords {Expander.Owner.Keywords.GetHashCode()}");
        // Console.WriteLine($"define-syntax added {kw.Identifier.Symbol} to keywords");
            
    }

    public ParsedForm Expand(Syntax stx) => Expander.Expand(stx, this);

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