using Jig.Expansion;
namespace Jig;

public interface IEvaluator {
    
    public IRuntime Runtime { get; }
    public Expander Expander { get; }
    
    public uint Phase { get; }
    
    public IEvaluatorFactory Factory { get; }
    
    public IRuntimeEnvironment Variables { get; }
    
    public SyntaxEnvironment Keywords { get; }

    public void REPLEval(ContinuationAny k, Syntax syntax);

    public void Eval(ContinuationAny k, Syntax syntax, IRuntimeEnvironment env);
    
    public void EvalSequence(IEnumerable<Syntax> syntax, ExpansionContextType type,  ContinuationAny? k = null);

    public void Import(ILibrary library, int phase = 0);

    public void ImportKeywords(ParsedImportForm importForm);
    public void ImportVariables(ParsedImportForm importForm);
    public IExpansionRule EvaluateTransformerExpression(ParsedLambda transformerLambdaExpr, ExpansionContext context);
    
    // public Syntax ApplyTransformer(Jig.Expansion.Transformer transformer, Syntax syntax);
}

public interface IEvaluator<TRuntime> : IEvaluator where TRuntime : IRuntime {
    public new TRuntime Runtime { get; }
    
    
}
