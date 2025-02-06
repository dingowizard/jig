using Jig;
using Jig.IO;

namespace Tests.DLRRuntime.Core;

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
        var actual = Utilities.BareInterpret(input);
        Assert.AreEqual(actual, expected);
    }

    [TestMethod]
    public void IfWithoutElse() {
        var actual = Utilities.BareInterpret("(if #t 1)");
        Assert.AreEqual("1", actual);
    }

    [TestMethod]
    public void SimplestIf()
    {
        var actual = Utilities.BareInterpret("(if #t 1 0)");
        Assert.AreEqual("1", actual);
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
        var actual = Utilities.BareInterpretUsingReadSyntax(input);
        Assert.AreEqual(actual, expected);

    }

}
