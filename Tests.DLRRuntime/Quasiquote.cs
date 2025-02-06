namespace Tests.DLRRuntime.Core;

[TestClass]
public class Quasiquote {

    [TestMethod]
    [DataRow("(quasiquote ())", "()")]
    [DataRow("(quasiquote x)", "x")]
    [DataRow("(quasiquote (a b c))", "(a b c)")]
    [DataRow("(quasiquote (a (+ 1 1) c))", "(a (+ 1 1) c)")]
    [DataRow("(quasiquote (a (unquote (+ 1 1)) c))", "(a 2 c)")]
    public void QuasiquoteEvaluated(string input, string expected) {
        var actual = Utilities.BareInterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(quasiquote (unquote b))", "2")]
    [DataRow("(quasiquote (a (unquote b) c))", "(a 2 c)")]
    public void QuasiquoteUnquoteWithBEquals2Evaluated(string input, string expected) {
        IInterpreter interp = new Interpreter();
        var actual = interp.InterpretSequenceReadSyntax(new string [] {
                "(define b 2)",
                input
            });
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(quasiquote ((unquote-splicing (cons a (cons b '()))) 3 4))", "(1 2 3 4)")]
    [DataRow("(quasiquote (-1 0 (unquote-splicing (cons a (cons b '()))) 3 4))", "(-1 0 1 2 3 4)")]
    public void QuasiquoteUnquoteSplicingWithA1B2Evaluated(string input, string expected) {
        IInterpreter interp = new Interpreter();
        var actual = interp.InterpretSequenceReadSyntax(new string [] {
                "(define a 1)",
                "(define b 2)",
                input
            });
        Assert.AreEqual(expected, actual);
    }
}
