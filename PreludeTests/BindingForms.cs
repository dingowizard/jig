namespace PreludeTests;

[TestClass]
public class BindingForms
{
    [TestMethod]
    [DataRow("(let ((x 1)) x)", "1")]
    [DataRow("(let ((x 1) (y 2)) (+ x y))", "3")]
    [DataRow("(let ((x 1) (y 2) (kons cons)) (kons x y))", "(1 . 2)")]
    [DataRow("(let ((x 1) (y 2) (cons +)) (cons x y))", "3")]
    public void Let(string input, string expected) {
        string actual = Utilities.PreludeInterp.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(let* () 1)", "1")]
    [DataRow("(let* ((a 1)) a)", "1")]
    [DataRow("(let* ((a 0) (b (+ a 1))) (+ a b))", "1")]
    public void LetStar(string input, string expected) {
        var actual = Utilities.PreludeInterp.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(letrec ((len (lambda (xs) (if (null? xs) 0 (+ 1 (len (cdr xs))))))) (len '(a b c d e)))", "5")]
    public void LetRec(string input, string expected) {
        string actual = Utilities.PreludeInterp.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }
}
