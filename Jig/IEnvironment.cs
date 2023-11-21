namespace Jig;

public interface IEnvironment {

    void LookUp(Continuation k, Expr.Symbol symbol);

    void LookUpSyntax(Continuation k, SyntaxObject.Identifier id);

}
