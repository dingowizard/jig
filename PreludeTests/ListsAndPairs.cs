namespace PreludeTests;

[TestClass]
public class ListsAndPairs
{
    [TestMethod]
    [DataRow("(list? (cons 1 2))", "#f")]
    [DataRow("(list? (cons 1 (list)))", "#t")]
    [DataRow("(list? (cons (list 1 2 3) '()))", "#t")]
    [DataRow("(list? (cons (list 1 2 3) '(1 . 2)))", "#f")]
    [DataRow("(list? (list 1 2 3 4))", "#t")]
    public void ListMakesListAndConsMight(string input, string expected) {

        string actual = Utilities.PreludeInterp.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(list)", "()")]
    [DataRow("(list 1 2 3)", "(1 2 3)")]
    public void ListConstructor(string input, string expected) {

        string actual = Utilities.PreludeInterp.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(length '())", "0")]
    [DataRow("(length '(a b c))", "3")]
    public void Length(string input, string expected) {

        string actual = Utilities.PreludeInterp.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(list-tail '() 0)", "()")]
    [DataRow("(list-tail '(1 2 3) 0)", "(1 2 3)")]
    [DataRow("(list-tail '(1 2 3) 1)", "(2 3)")]
    [DataRow("(list-tail '(1 2 3) 2)", "(3)")]
    public void ListTail(string input, string expected) {

        string actual = Utilities.PreludeInterp.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(list-ref '(a b c) 0)", "a")]
    [DataRow("(list-ref '(a b c) 1)", "b")]
    [DataRow("(list-ref '(a b c) 2)", "c")]
    public void ListRef(string input, string expected) {

        string actual = Utilities.PreludeInterp.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(map (lambda (n) (* n n)) '(1 2 3 4 5 6 7 8 9 10))", "(1 4 9 16 25 36 49 64 81 100)")]
    [DataRow("(map (lambda (n) (* n n)) '())", "()")]
    [DataRow("(map + '(1 2 3 4 5) '(5 4 3 2 1))", "(6 6 6 6 6)")]
    [DataRow("(map (lambda (x y z) (* z (+ x y))) '(1 2 3 4 5) '(5 4 3 2 1) '(1 2 3 4 5))", "(6 12 18 24 30)")]
    public void Map(string input, string expected) {

        string actual = Utilities.PreludeInterp.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(fold + 0 '(1 2 3 4 5 6 7 8 9 10))", "55")]
    [DataRow("(fold cons '() '(1 2 3 4 5 6 7 8 9 10))", "(10 9 8 7 6 5 4 3 2 1)")]
    [DataRow("(fold * 1 '(1 2 3 4 5 6 7))", "5040")]
    public void Fold(string input, string expected) {

        string actual = Utilities.PreludeInterp.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(reverse '())", "()")]
    [DataRow("(reverse '(1 2 3 4 5))", "(5 4 3 2 1)")]
    public void Reverse(string input, string expected) {

        string actual = Utilities.PreludeInterp.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(caar '((1 2) 3))", "1")]
    public void Caar(string input, string expected) {

        string actual = Utilities.PreludeInterp.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(cdar '((1 2) 3))", "(2)")]
    public void Cdar(string input, string expected) {

        string actual = Utilities.PreludeInterp.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(cadr '(1 2 3))", "2")]
    public void Cadr(string input, string expected) {

        string actual = Utilities.PreludeInterp.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(cddr '(1 2 3))", "(3)")]
    public void Cddr(string input, string expected) {
        string actual = Utilities.PreludeInterp.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(cdddr '(1 2 3))", "()")]
    public void Cdddr(string input, string expected) {
        string actual = Utilities.PreludeInterp.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(caddr '(1 2 3))", "3")]
    public void Caddr(string input, string expected) {
        string actual = Utilities.PreludeInterp.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }
}
