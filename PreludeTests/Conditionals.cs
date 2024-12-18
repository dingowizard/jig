
namespace PreludeTests;

[TestClass]
public class Conditionals {



    [TestMethod]
    [DataRow("(cond (#f 1) (#t 2))", "2")]
    [DataRow("(cond ((null? (list 1 2)) 1) ((= 0 1) 2) (#t 3))", "3")]
    public void BasicCond(string input, string expected) {
        var actual = Utilities.PreludeInterp.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(when #t 2)", "2")]
    [DataRow("(when #t 0 1 2)", "2")]
    // TODO: the following test will fail in chez because the define is in an invalid context
    // seems a bit strange though
    // [DataRow("(when (null? (list)) (define z 25) (set! z (+ 1 z)) z)", "26")]
    public void When(string input, string expected) {
        var actual = Utilities.PreludeInterp.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(case (+ 3 4) ((1 3 5 7 9) 'odd) ((0 2 4 6 8) 'even))", "odd")]
    [DataRow("(case (+ 3 4) ((1 3 5 7 9) 'odd) ((0 2 4 6 8) 'even) (else 'out-of-range))", "odd")]
    public void Case(string input, string expected) {
        var actual = Utilities.PreludeInterp.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }
}
