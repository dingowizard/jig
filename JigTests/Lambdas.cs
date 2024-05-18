using Jig;
using Jig.IO;

namespace JigTests.Core;

[TestClass]
public class Lambdas
{
    [TestMethod]
    public void IDEvaluatesToProcedure()
    {
        Expr result = List.Empty;
        Continuation.OneArgDelegate setResult = (x) => {result = x; return null;};
        Expr? expr = Jig.Reader.Reader.Read(InputPort.FromString("(lambda (x) x)"));
        Assert.IsNotNull(expr);
        Program.Eval(setResult, expr, new Jig.Environment());
        Assert.IsInstanceOfType(result, typeof(Procedure));

    }
    [TestMethod]
    public void ApplySimpleLambdaExprToInt() {
        string actual = Utilities.BareInterpret("((lambda (x) x) 1)");
        Assert.AreEqual("1", actual);
    }

    [TestMethod]
    [DataRow("((lambda () 1))", "1")]
    [DataRow("((lambda (p) (car p)) (cons 1 2))", "1")]
    [DataRow("((lambda (p) (car (cdr p))) (cons 1 (quote (2 3))))", "2")]
    [DataRow("((lambda (a b) b) (cons 3 4) (cons 1 (quote (2))))", "(1 2)")]
    [DataRow("((lambda (x) (cons 1 2) (cdr (cons 1 2)) #f 12 x) (cons 1 (quote (2))))", "(1 2)")]
    [DataRow("((lambda (a b c d e f g) g) 1 2 3 4 5 6 7)", "7")]
    [DataRow("((lambda x x) 1 2 3 4)", "(1 2 3 4)")]
    [DataRow("((lambda (x . rest) rest) 1 2 3 4)", "(2 3 4)")]
    [DataRow("((lambda (x y . rest) rest) 1 2 3 4)", "(3 4)")]
    [DataRow("((lambda (x y z . rest) rest) 1 2 3 4)", "(4)")]
    [DataRow("((lambda (x y z a . rest) rest) 1 2 3 4)", "()")]
    [DataRow("((lambda (x y . rest) (cons y (cons x rest))) 1 2 3 4)", "(2 1 3 4)")]
    public void ApplyLambdaExpr(string input, string expected)
    {
        string actual = Utilities.BareInterpret(input);
        Assert.AreEqual(expected, actual);

    }

    [TestMethod]
    [DataRow("((lambda (x) x) 1)", "1")]
    [DataRow("((lambda (p) (car p)) (cons 1 2))", "1")]
    [DataRow("((lambda (p) (car (cdr p))) (cons 1 (quote (2 3))))", "2")]
    [DataRow("((lambda (a b) b) (cons 3 4) (cons 1 (quote (2))))", "(1 2)")]
    [DataRow("((lambda (x) (cons 1 2) (cdr (cons 1 2)) #f 12 x) (cons 1 (quote (2))))", "(1 2)")]
    [DataRow("((lambda (a b c d e f g) g) 1 2 3 4 5 6 7)", "7")]
    [DataRow("((lambda x x) 1 2 3 4)", "(1 2 3 4)")]
    [DataRow("((lambda (x . rest) rest) 1 2 3 4)", "(2 3 4)")]
    [DataRow("((lambda (x y . rest) rest) 1 2 3 4)", "(3 4)")]
    [DataRow("((lambda (x y z . rest) rest) 1 2 3 4)", "(4)")]
    [DataRow("((lambda (x y z a . rest) rest) 1 2 3 4)", "()")]
    [DataRow("((lambda (x y . rest) (cons y (cons x rest))) 1 2 3 4)", "(2 1 3 4)")]
    public void ApplyLambdaExprSyntax(string input, string expected)
    {
        var actual = Utilities.BareInterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);

    }
}
