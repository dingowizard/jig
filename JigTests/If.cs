using Jig;
using Jig.IO;

namespace JigTests;

[TestClass]
public class If
{
    [TestMethod]
    [DataRow("(if #f 0 1)", "1")]
    [DataRow("(if (null? (quote ())) #t #f)", "#t")]
    [DataRow("(if #t (car (cons #t #f)) (cdr (cons #t #f)))", "#t")]
    [DataRow("(if #f (car (cons #t #f)) (cdr (cons #t #f)))", "#f")]
    public void Ifs(string input, string expected)
    {
        var actual = Utilities.Interpret(input);
        Assert.AreEqual(actual, expected);

    }

    [TestMethod]
    [DataRow("(if #f 0 1)", "1")]
    [DataRow("(if 1 0 1)", "0")]
    [DataRow("(if (quote b) 0 1)", "0")]
    [DataRow("(if \"hello\" 0 1)", "0")]
    [DataRow("(if 1.12 0 1)", "0")]
    [DataRow("(if (null? (quote ())) #t #f)", "#t")]
    [DataRow("(if #t (car (cons #t #f)) (cdr (cons #t #f)))", "#t")]
    [DataRow("(if #f (car (cons #t #f)) (cdr (cons #t #f)))", "#f")]
    public void IfsSyntax(string input, string expected)
    {
        var actual = Utilities.InterpretUsingReadSyntax(input);
        Assert.AreEqual(actual, expected);

    }

}
