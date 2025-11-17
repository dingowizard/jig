using Jig;

namespace VM;

public class Primitive2 : SchemeValue {

    public Primitive2(PrimitiveProcedure proc, int required, bool hasRest) {
        Procedure = proc;
        Required = required;
        HasRest = hasRest;
        NumVarsForScope = Required + (HasRest ? 1 : 0) ;

    }
    
    public Primitive2(PrimitiveProcedure proc, int required, bool hasRest, int numVarsForScope) {
        Procedure = proc;
        Required = required;
        HasRest = hasRest;
        NumVarsForScope = numVarsForScope;

    }
    public override string Print() => "#<procedure>";
    
    
    public PrimitiveProcedure Procedure { get; }

    public bool HasRest { get; }
    public int Required { get;  }
    public int NumVarsForScope { get;  }
}