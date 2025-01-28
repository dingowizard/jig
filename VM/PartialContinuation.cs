using Jig;

namespace VM;

public class PartialContinuation : Form {
    
    public PartialContinuation(
        Template template,
        ulong returnAddress,
        Environment environment,
        List evalStack,
        PartialContinuation continuation)
    {
        Template = template;
        Continuation = continuation;
        ReturnAddress = returnAddress;
        Environment = environment;
        EvalStack = evalStack;
    }

    public Template Template { get; }

    public PartialContinuation Continuation { get; }
    
    public ulong ReturnAddress { get; }
    
    public Environment Environment { get; }
    
    public Jig.List EvalStack { get; }

    public override string Print() => "#<continuation>";
}