namespace Tests.Common;

    
[TestClass]
public abstract class ListsAndPairsBase {
    
    protected abstract IInterpreter Interp { get; set; }
    
    [TestMethod]
    [DataRow("(cons 1 (quote (2)))", "(1 2)")]
    [DataRow("(cons #t #f)", "(#t . #f)")]
    [DataRow("(cons 1 (cons 2 (cons 3 (quote ()))))", "(1 2 3)")]
    [DataRow("(cons #f (cons #f (cons #t #f)))", "(#f #f #t . #f)")]
    [DataRow("(cons (quote a) (quote (b c)))", "(a b c)")]
    public void Cons(string input, string expected) {
        var actual = Interp.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(car (quote (1 2)))", "1")]
    [DataRow("(car (quote (a . b)))", "a")]
    public void Car(string input, string expected) {
        var actual = Interp.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    // [TestMethod]
    // [DataRow("(pair? (cons 1 2))", "#t")]
    // [DataRow("(pair? (cons 1 '()))", "#t")]
    // [DataRow("(pair? (cons '(1 2 3) '()))", "#t")]
    // public void ConsMakesPairs(string input, string expected) {
    //     var actual = Interp.InterpretUsingReadSyntax(input);
    //     Assert.AreEqual(expected, actual);
    // }

    [TestMethod]
    [DataRow("(cdr (quote (1 2)))", "(2)")]
    [DataRow("(cdr (quote (a . b)))", "b")]
    public void Cdr(string input, string expected) {
        var actual = Interp.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(car (cdr (cons (quote a) (cons (quote b) (cons (quote c) (quote ()))))))", "b")]
    [DataRow("(car (car (cons (cons 1 (cons 2 (quote ()))) (cons 3 (cons 4 (quote ()))))))", "1")]
    public void ConsCarCdr(string input, string expected) {
        var actual = Interp.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(null? (quote ()))", "#t")]
    [DataRow("(null? (quote (1)))", "#f")]
    [DataRow("(null? (quote (1 2)))", "#f")]
    [DataRow("(null? (cons 1 2))", "#f")]
    [DataRow("(null? (quote b))", "#f")]
    public void NullP(string input, string expected) {
        var actual = Interp.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    // [TestMethod]
    // [DataRow("(append)", "()")]
    // [DataRow("(append '(1 2 3) '())", "(1 2 3)")]
    // [DataRow("(append '() '(1 2 3))", "(1 2 3)")]
    // [DataRow("(append '(1 2 3) '(4 5 6))", "(1 2 3 4 5 6)")]
    // [DataRow("(append 5)", "5")]
    // [DataRow("(append '() 5)", "5")]
    // [DataRow("(append '(1 2 3 4) 5)", "(1 2 3 4 . 5)")]
    // [DataRow("(append '(1 2 3) '(4 5 6) '(7 8 9))", "(1 2 3 4 5 6 7 8 9)")]
    // [DataRow("(append '(1 2 3) '(4 5 6) '())", "(1 2 3 4 5 6)")]
    // [DataRow("(append '() '(1 2 3) '(4 5 6))", "(1 2 3 4 5 6)")]
    // [DataRow("(append '(1 2 3) '(4 5 6) 7)", "(1 2 3 4 5 6 . 7)")]
    // public void Append(string input, string expected) {
    //     var actual = Interp.InterpretUsingReadSyntax(input);
    //     Assert.AreEqual(expected, actual);
    // }
}