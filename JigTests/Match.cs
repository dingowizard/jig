namespace JigTests;

[TestClass]
public class Match {
    [TestMethod]
    [DataRow("(match 1 (x x))", "1")]
    [DataRow("(match 1 (x (+ x 1)))", "2")]
    [DataRow("(match 1 (x 1 3 4 5 (+ x 1)))", "2")]
    [DataRow("(match 1 (x (set! x 2) (+ x 1)))", "3")]
    [DataRow("(match (cons 1 2) ((a . b) a))", "1")]
    [DataRow("(match (cons 1 2) ((a . b) b))", "2")]
    [DataRow("(match (cons 1 2) ((a . b) 'pair) (x 'anything))", "pair")]
    [DataRow("(match 1 ((a . b) 'pair) (x 'anything))", "anything")]
    [DataRow("(match '() (() 'null))", "null")]
    [DataRow("(match 0 (0 'zero))", "zero")]
    [DataRow("(match (cons 1 (cons 2 3)) ((a . (b . c)) (+ a b c)))", "6")]
    [DataRow("(match '(1 2 . 3) ((a . (b . c)) c))", "3")]
    [DataRow("(match '(1 2 . 3) ((a b . c) c))", "3")]
    [DataRow("(match (list 1 2 3) ((a b c) (+ a b c)))", "6")]
    [DataRow("(match (cons (cons 1 2) 3) (((a . b) . c) (+ a b c)))", "6")] // stack overflow
    public void MatchExamples(string input, string expected) {
        var actual = Utilities.BareInterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

}