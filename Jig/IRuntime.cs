using Jig.Expansion;
namespace Jig;

public interface IRuntime {
    Syntax ApplyTransformer(Jig.Expansion.Transformer transformer, Syntax syntax);

    IExpansionRule EvaluateTransformerExpression(ParsedLambda transformerLambdaExpr, ExpansionContext context);
    
    IRuntimeEnvironment RuntimeEnvironment { get; }
}