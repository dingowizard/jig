
using Jig;
using Jig.IO;

namespace JigTests;

[TestClass]
public class ListsAndPairs
{
    [TestMethod]
    [DataRow("(cons 1 (quote (2)))", "(1 2)")]
    [DataRow("(cons #t #f)", "(#t . #f)")]
    [DataRow("(cons 1 (cons 2 (cons 3 (quote ()))))", "(1 2 3)")]
    [DataRow("(cons #f (cons #f (cons #t #f)))", "(#f #f #t . #f)")]
    [DataRow("(cons (quote a) (quote (b c)))", "(a b c)")]
    public void Cons(string input, string expected) {
        string actual = "";
        Continuation setResult = (x) => actual = x.Print();
        SyntaxObject? stx = Jig.Reader.Reader.ReadSyntax(InputPort.FromString(input));
        Assert.IsNotNull(stx);
        Program.Eval(setResult, stx, new Jig.Environment());
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(car (quote (1 2)))", "1")]
    [DataRow("(car (quote (a . b)))", "a")]
    public void Car(string input, string expected) {
        string actual = "";
        Continuation setResult = (x) => actual = x.Print();
        SyntaxObject? stx = Jig.Reader.Reader.ReadSyntax(InputPort.FromString(input));
        Assert.IsNotNull(stx);
        Program.Eval(setResult, stx, new Jig.Environment());
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(cdr (quote (1 2)))", "(2)")]
    [DataRow("(cdr (quote (a . b)))", "b")]
    public void Cdr(string input, string expected) {
        string actual = "";
        Continuation setResult = (x) => actual = x.Print();
        SyntaxObject? stx = Jig.Reader.Reader.ReadSyntax(InputPort.FromString(input));
        Assert.IsNotNull(stx);
        Program.Eval(setResult, stx, new Jig.Environment());
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(car (cdr (cons (quote a) (cons (quote b) (cons (quote c) (quote ()))))))", "b")]
    [DataRow("(car (car (cons (cons 1 (cons 2 (quote ()))) (cons 3 (cons 4 (quote ()))))))", "1")]
    public void ConsCarCdr(string input, string expected) {
        string actual = "";
        Continuation setResult = (x) => actual = x.Print();
        SyntaxObject? stx = Jig.Reader.Reader.ReadSyntax(InputPort.FromString(input));
        Assert.IsNotNull(stx);
        Program.Eval(setResult, stx, new Jig.Environment());
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(null? (quote ()))", "#t")]
    [DataRow("(null? (quote (1)))", "#f")]
    [DataRow("(null? (quote (1 2)))", "#f")]
    [DataRow("(null? (cons 1 2))", "#f")]
    [DataRow("(null? (quote b))", "#f")]
    public void NullP(string input, string expected) {
        string actual = "";
        Continuation setResult = (x) => actual = x.Print();
        SyntaxObject? stx = Jig.Reader.Reader.ReadSyntax(InputPort.FromString(input));
        Assert.IsNotNull(stx);
        Program.Eval(setResult, stx, new Jig.Environment());
        Assert.AreEqual(expected, actual);
    }


}
