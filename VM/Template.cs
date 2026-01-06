using Jig;
using Jig.Expansion;

namespace VM;

public class Template : SchemeValue { // TODO: is this a scheme value? should it ever be returned by eval?
    public Template(int numVarsForScope, ulong[] code, Parameter[] vars, SchemeValue[] lits, int requiredParameterCount, bool hasRestParameter) {
        Literals = lits;
        Vars = vars; // top vars and lexical vars declared at scopes outside this one
        Code = code;
        NumVarsForScope = numVarsForScope;
        RequiredParameterCount = requiredParameterCount;
        HasRestParameter = hasRestParameter;
    }

    public Template(ulong[] code, Parameter[] vars, SchemeValue[] lits) {
        Literals = lits;
        Vars = vars; // top vars and lexical vars declared at scopes outside this one
        Code = code;
        NumVarsForScope = 0;
        RequiredParameterCount = 0;
        HasRestParameter = false;
    }

    public Identifier Name { get; set; } = new Identifier(new Symbol("#<anonymous>"));
    
    public static Template Empty => new Template(0, [], [], [], 0, false);
    
    public int RequiredParameterCount { get; }
    
    public bool  HasRestParameter { get; }
    
    public int NumVarsForScope { get; }
    public Jig.SchemeValue[] Literals { get; }
    
    public Parameter[] Vars { get; }
    public ulong[] Code { get; }

    public override string Print() => "<#lambda template>";
}