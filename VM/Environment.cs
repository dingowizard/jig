using System.Diagnostics;
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

        return LexicalVars[index].Value ?? throw new Exception("GetArg: index = {index}. Not initialized.");
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
    
    public Environment ExtendForProcCall(Procedure proc, IEnumerable<SchemeValue> args) {
        return new Environment(this, new Frame(this, args, proc));
    }
    
    
    public Environment ExtendForProcCall(Template template, IEnumerable<SchemeValue> args) {
        return new Environment(this, new Frame(this, args, template));
    }
    
    public void BindParameter(ulong p0, SchemeValue val) {
        Debug.Assert(LexicalVars is not null);
        LexicalVars[p0] = new Location(val);
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
        
        ArraySegment<Location> InitializedLocations { get; }

        public Location this[ulong index] {
            get => Locations[index];
            set => Locations[index] = value;
        }

        public Location GetAt(int scopeLevel, int index)
        {
            var frame = this;
            if (scopeLevel < ScopeLevel) {
                throw new Exception();
                
            }
            while (frame.ScopeLevel < scopeLevel) {
                frame = frame.Parent;
                scopeLevel--;
            }

            if (frame.Locations[index] is null) {
                throw new Exception($"In GetAt: trying to get frame index {index} but it was never initialized.");
            }
            return frame.Locations[index];

        }
        
        private Frame? Parent { get; }

        public Frame(Environment parent, IEnumerable<SchemeValue> values) {
            // TODO: use this when binding args in procedure calls
            // NOTE: but you'd actually need one that also takes
            // the total number of locals as an argument,
            // then sets up the backing array and the arraysegment (the inititialized vars)
            Parent = parent.LexicalVars;
            ScopeLevel = parent.ScopeLevel + 1;
            Locations = values.Select(v =>  new Location(v)).ToArray();
        }

        public Frame(Environment parent, int number) {
            // TODO: new constructor that takes values of arguments
            // and this same number.
            // creates locations only for the ones with values
            // then DefArg will create the locations
            // locations will not have nullable values
            Parent = parent.LexicalVars;
            ScopeLevel = parent.ScopeLevel + 1;
            Locations = new Location[number];

        }
        public Frame(Environment parent, IEnumerable<SchemeValue> args, Template template) {
            Parent = parent.LexicalVars;
            ScopeLevel = parent.ScopeLevel + 1;
            Locations = new Location[template.NumVarsForScope];
            SchemeValue[] arrayArgs = args.ToArray();
            int index = 0;
            if (template.RequiredParameterCount > 0) {
                for (; index < template.RequiredParameterCount; index++) {
                    Locations[index] = new Location(arrayArgs[index]);
                }
                
            }
            if (template.HasRestParameter) {
                Locations[index] = new Location(args.Skip(index).ToJigList());
                index++;
            }

            while (index < template.NumVarsForScope) {
                // TODO: we need to create new locations for any number of
                // lambda body defines. For example:
                // (lambda ()
                //     (define odd? <use even?>)
                //     (define even? <use odd?>)
                //     (even? 15))
                // odd? needs a location for even in order to evaluate its rhs
                Locations[index++] = new Location();
                
            }
        }

        public Frame(Environment parent, IEnumerable<SchemeValue> args, Procedure proc) : this (parent, args, proc.Template) { }
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
        if (!_topLevels.TryAdd(p, runtimeBinding)) {
            throw new Exception($"DefineTopLevel: trying to add {p.Symbol.Print()} but it is already in _topLevels");

        }
    }

    public void DefArg(ulong ir, SchemeValue v) {
        if (LexicalVars is null) {
            throw new Exception($"DefArg isn't supposed to be used at toplevel");
        }
        Debug.Assert(LexicalVars[ir] is not null);
        LexicalVars[ir].Value = v;
    }
}