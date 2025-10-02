using Jig;

namespace VM;

public class Template : SchemeValue {
    public Template(int numVarsForScope, ulong[] code, Binding[] bindings, SchemeValue[] lits, int requiredParameterCount, bool hasRestParameter) {
        Slots = lits;
        Bindings = bindings;
        Code = code;
        NumVarsForScope = numVarsForScope;
        RequiredParameterCount = requiredParameterCount;
        HasRestParameter = hasRestParameter;
    }

    public static Template Empty => new Template(0, [], [], [], 0, false);
    
    public int RequiredParameterCount { get; }
    
    public bool  HasRestParameter { get; }
    
    public int NumVarsForScope { get; }
    public Jig.SchemeValue[] Slots { get; }
    
    public Binding[] Bindings { get; }
    public ulong[] Code { get; }

    public override string Print() => "<#lambda template>";
}