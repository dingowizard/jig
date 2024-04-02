namespace JigTests;

[TestClass]
public class Macros {

    [TestMethod]
    [DataRow("(and #f 26)", "#f")]
    [DataRow("(and)", "#t")]
    [DataRow("(and 1)", "1")]
    [DataRow("(and 1 2 3)", "3")]
    [DataRow("(and #f #f #f 3)", "#f")]
    [DataRow("(and 1 #f 3)", "#f")]
    [DataRow("(and #f)", "#f")]
    [DataRow("(and #t)", "#t")]
    public void ApplyAndMacro(string input, string expected) {
        var actual = Utilities.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(or 1 2)", "1")]
    [DataRow("(or #f 2)", "2")]
    [DataRow("(or #f #f)", "#f")]
    public void ApplyOrMacro(string input, string expected) {
        var actual = Utilities.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(let ((x 1)) x)", "1")]
    public void LetMacro(string input, string expected) {
        var actual = Utilities.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(or 1 (oops!))", "1")]
    public void OrShortCircuits(string input, string expected) {
        var actual = Utilities.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(and 1 #f (oops!))", "#f")]
    public void AndShortCircuits(string input, string expected) {
        var actual = Utilities.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(let* () 1)", "1")]
    [DataRow("(let* ((a 1)) a)", "1")]
    [DataRow("(let* ((a 0) (b (+ a 1))) (+ a b))", "1")]
    public void LetStar(string input, string expected) {
        var actual = Utilities.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(cond (#f 1) (#t 2))", "2")]
    [DataRow("(cond ((null? (list 1 2)) 1) ((= 0 1) 2) (#t 3))", "3")]
    public void BasicCond(string input, string expected) {
        var actual = Utilities.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(when #t 2)", "2")]
    [DataRow("(when (null? (list)) (define z 25) (set! z (+ 1 z)) z)", "26")]
    public void When(string input, string expected) {
        var actual = Utilities.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

}
