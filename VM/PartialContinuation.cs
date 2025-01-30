using Jig;

namespace VM;

public abstract class Continuation : Form {
    public override string Print() => "#<continuation";

    public abstract ulong ReturnAddress { get; }
    
    public abstract Environment Environment { get; }
    
    public abstract Jig.List EvalStack { get; }
}

public class TopLevelContinuation : Continuation {
    public TopLevelContinuation(Environment env) {
        Environment = env;
    }

    public override ulong ReturnAddress { get; } = ulong.MaxValue;
    public override Environment Environment { get; }

    public override Jig.List EvalStack { get; } = Jig.List.Null;
}

public class PartialContinuation : Continuation {
    
    public PartialContinuation(
        Template template,
        ulong returnAddress,
        Environment environment,
        List evalStack,
        Continuation continuation)
    {
        Template = template;
        Continuation = continuation;
        ReturnAddress = returnAddress;
        Environment = environment;
        EvalStack = evalStack;
    }

    public Template Template { get; }

    public Continuation Continuation { get; }
    
    public override ulong ReturnAddress { get; }
    
    public override Environment Environment { get; }
    
    public override Jig.List EvalStack { get; }

}