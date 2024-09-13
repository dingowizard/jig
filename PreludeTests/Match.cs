namespace PreludeTests;

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
    [DataRow("(match 'a ('b #f) ('a #t))", "#t")]
    [DataRow("(match '(a 1) ('a #f) (('a n) n) )", "1")]
    [DataRow("(match '(a 1 2) ('a #f) (('a n) n) (('a 2 n) (* 2 n)) (('a 1 n) (* n 3)) )", "6")]
    [DataRow("(match (cons 1 (cons 2 3)) ((a . (b . c)) (+ a b c)))", "6")]
    [DataRow("(match '(1 2 . 3) ((a . (b . c)) c))", "3")]
    [DataRow("(match '(1 2 . 3) ((a b . c) c))", "3")]
    [DataRow("(match (list 1 2 3) ((a b c) (+ a b c)))", "6")]
    [DataRow("(match (list 1 2 3 4 5) ((a b . c) c))", "(3 4 5)")]
    [DataRow("(match (cons (cons 1 2) 3) (((a . b) . c) (+ a b c)))", "6")]
    public void MatchExamples(string input, string expected) {
        var actual = Utilities.PreludeInterp.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

}