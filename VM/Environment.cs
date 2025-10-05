using Jig;
using Jig.Expansion;
namespace VM;

public class Environment : SchemeValue, IRuntimeEnvironment {
    private int ScopeLevel { get; }

    private Dictionary<Parameter, Binding> _topLevels { get; }
    
    public Location LookUpLocation(Parameter parameter) {
        
        if (parameter.ScopeLevel == 0) {
            if (_topLevels.TryGetValue(parameter, out var binding)) {
                return binding.Location;
            }

            throw new Exception($"couldn't find toplevel var {parameter.Symbol} in env. keys in toplevels are: {string.Join(", ", _topLevels.Keys.Select(p => p.Symbol.Print()))}");
        }

        return GetLexVarLocation(parameter.ScopeLevel, parameter.Index);
    }

    public void SetArg(ulong ir, SchemeValue val)
    {
        if (LexicalVars is null)
        {
            throw new Exception();
        }

        LexicalVars[ir].Value = val;
    }

    public int ArgsLength => LexicalVars.Length;
    public SchemeValue GetArg(ulong index) {
        if (LexicalVars is null)
        {
            throw new Exception();
        }

        return LexicalVars[index].Value ?? throw new Exception("undefined");
    }

    public Environment(IEnumerable<Jig.Binding> topVars)
    {
        ScopeLevel = 0;
        _topLevels = new();
        foreach (var topVar in topVars) {
            _topLevels.Add(topVar.Parameter, topVar);
        }

        LexicalVars = null;
    }

    private Environment(Environment parent, Frame newFrame)
    {
        _topLevels = parent._topLevels;
        LexicalVars = newFrame;
        ScopeLevel = parent.ScopeLevel + 1;

    }

    public Environment Extend(int number)
    {
        return new Environment(this, new Frame(this, number));

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
        
        public int Length => Locations.Length;
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

        public Frame(Environment parent, int number)
        {
            Parent = parent.LexicalVars;
            ScopeLevel = parent.ScopeLevel + 1;
            Locations = Enumerable.Range(0, number).Select(_ => new Location()).ToArray();

        }
    }

    public override string Print() => "#<environment>";

    public Dictionary<Parameter, Binding> TopLevels => _topLevels;
    public static Environment Default { get; set; }

    public static Environment Minimal()
    {
        // TODO: Minimal and Default are the same
        return new Environment([]);
    }
    static Environment() {
        Default = new Environment([]);
    }
    public void DefineTopLevel(Parameter p, Binding runtimeBinding)
    {
        if (_topLevels.ContainsKey(p)) {
            throw new Exception($"DefineTopLevel: trying to add {p.Symbol.Print()} but it is already in _topLevels");

        }
        _topLevels.Add(p, runtimeBinding);
    }

}