using Jig.Expansion;
namespace Jig;

public interface IEvaluator {
    
    public IRuntime Runtime { get; }
    public Expander Expander { get; }
    
    public uint Phase { get; }
    
    public IEvaluatorFactory Factory { get; }
    
    public IRuntimeEnvironment Variables { get; }
    
    public SyntaxEnvironment Keywords { get; }

    public void Eval(ContinuationAny k, Syntax syntax);

    public void Eval(ContinuationAny k, Syntax syntax, IRuntimeEnvironment env);
    
    public void EvalSequence(ContinuationAny continuation, IEnumerable<Syntax> syntax);

    public void Import(ILibrary library, uint phase = 0);
    public IExpansionRule EvaluateTransformerExpression(ParsedLambda transformerLambdaExpr, ExpansionContext context);
    
    // public Syntax ApplyTransformer(Jig.Expansion.Transformer transformer, Syntax syntax);
}

public interface IEvaluator<TRuntime> : IEvaluator where TRuntime : IRuntime {
    public new TRuntime Runtime { get; }
    
    
}
