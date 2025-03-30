using Jig;

namespace VM;

public abstract class Continuation : Form {
    public override string Print() => "#<continuation";

    public abstract ulong ReturnAddress { get; }
    
    public abstract int Required { get; }
    
    public abstract bool HasOptional { get; }
    
    public abstract Environment Environment { get; }
    
}

public class TopLevelContinuation : Continuation {
    public TopLevelContinuation(Environment env) {
        Environment = env;
    }
    
    public override int Required => 0;
    
    public override bool HasOptional => true;

    public override ulong ReturnAddress { get; } = ulong.MaxValue;
    public override Environment Environment { get; }

}

public class PartialContinuation : Continuation {
    
    public PartialContinuation(
        Template template,
        ulong returnAddress,
        Environment environment,
        uint sp,
        uint fp,
        Continuation continuation,
        int requiredValues,
        bool hasOptional)
    {
        Template = template;
        Continuation = continuation;
        ReturnAddress = returnAddress;
        SP = sp;
        FP = fp;
        Environment = environment;
        Required = requiredValues;
        HasOptional = hasOptional;
    }

    public Template Template { get; }

    public Continuation Continuation { get; }

    public uint SP;
    public uint FP;
    public override ulong ReturnAddress { get; }
    public override int Required { get; }
    public override bool HasOptional { get; }

    public override Environment Environment { get; }
    

}
public class PartialContinuationForCallWithValues : PartialContinuation {
    public PartialContinuationForCallWithValues(
        Template continuationProcTemplate,
        ulong i,
        Environment envt,
        uint sp,
        uint fp,
        Continuation cont,
        int continuationProcRequired,
        bool continuationProcHasRest)
        : base(continuationProcTemplate, i, envt, sp, fp, cont, continuationProcRequired, continuationProcHasRest) {
        
    }
}
