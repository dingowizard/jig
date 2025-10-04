using Jig;
using Jig.Expansion;
namespace VM;

public class Environment2 : SchemeValue, IRuntimeEnvironment {
    private int ScopeLevel { get; }

    private Dictionary<Parameter, Jig.Binding> _topLevels { get; }
    
    public Location LookUpLocation(Parameter parameter) {
        
        if (parameter.ScopeLevel == 0) {
            if (_topLevels.TryGetValue(parameter, out Jig.Binding binding)) {
                return binding.Location;
            }

            throw new Exception($"couldn't find toplevel var {parameter.Symbol} in env");
        }

        return GetLexVarLocation(parameter.ScopeLevel, parameter.Index);
    }

    public SchemeValue GetArg(ulong index) {
        if (LexicalVars is null)
        {
            throw new Exception();
        }

        return LexicalVars[index].Value ?? throw new Exception("undefined");
    }

    public Environment2(IEnumerable<Jig.Binding> topVars)
    {
        ScopeLevel = 0;
        _topLevels = new();
        foreach (var topVar in topVars) {
            _topLevels.Add(topVar.Parameter, topVar);
        }

        LexicalVars = null;
    }

    private Environment2(Environment2 parent, Frame newFrame)
    {
        _topLevels = parent._topLevels;
        LexicalVars = newFrame;
        ScopeLevel = parent.ScopeLevel + 1;

    }

    public Environment2 Extend(int number)
    {
        return new Environment2(this, new Frame(this, number));

    }
    
    public void BindParameter(ulong p0, SchemeValue val) {
        LexicalVars[p0].Value = val;
    }
    
    private Frame? LexicalVars { get; }

    private Location GetLexVarLocation(int parameterScopeLevel, int parameterIndex) {
        if (LexicalVars is null) {
            throw new Exception();
            
        }
        return LexicalVars.GetAt(parameterScopeLevel, parameterIndex);
    }

    private class Frame {
        int ScopeLevel { get; }
        Location[]  Locations { get; }
        
        public Location this[ulong index] => Locations[index];

        public Location GetAt(int scopeLevel, int index)
        {
            var frame = this;
            if (scopeLevel < ScopeLevel) {
                throw new Exception();
                
            }
            while (frame.ScopeLevel < scopeLevel)
            {
                frame = frame.Parent;
                scopeLevel--;
            }
            return frame.Locations[index];

        }
        
        private Frame? Parent { get; }

        public Frame(Environment2 parent, int number)
        {
            Parent = parent.LexicalVars;
            ScopeLevel = parent.ScopeLevel + 1;
            Locations = Enumerable.Range(0, number).Select(_ => new Location()).ToArray();

        }
    }

    public override string Print() => "#<environment>";

    public Dictionary<Symbol, IRuntimeBinding> TopLevels { get; }
    public static Environment2 Default { get; set; }

    public static Environment2 Minimal()
    {
        // TODO: Minimal and Default are the same
        return new Environment2([]);
    }
    static Environment2() {
        Default = new Environment2([]);
    }
    public void DefineTopLevel(Symbol symbol, IRuntimeBinding runtimeBinding)
    {
        var parameter = new Parameter(symbol, 0, 0, null);
        _topLevels.Add(parameter, runtimeBinding);
    }
}