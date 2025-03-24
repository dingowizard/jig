using Jig;

namespace VM;

public class Template : Form {
    public Template(int numVarsForScope, ulong[] code, Binding[] bindings, Form[] lits, int requiredParameterCount, bool hasRestParameter) {
        Slots = lits;
        Bindings = bindings;
        Code = code;
        NumVarsForScope = numVarsForScope;
        RequiredParameterCount = requiredParameterCount;
        HasRestParameter = hasRestParameter;
    }
    
    public int RequiredParameterCount { get; }
    
    public bool  HasRestParameter { get; }
    
    public int NumVarsForScope { get; }
    public Jig.Form[] Slots { get; }
    
    public Binding[] Bindings { get; }
    public ulong[] Code { get; }

    public override string Print() => "<#lambda template>";
}