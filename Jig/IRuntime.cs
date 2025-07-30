namespace Jig;

public interface IRuntime {
    Syntax ApplyTransformer(Jig.Expansion.Transformer transformer, Syntax syntax);

    ITransformer EvaluateTransformerExpression(ParsedExpr transformerLambdaExpr);
}