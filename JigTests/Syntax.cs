namespace JigTests;

[TestClass]
public class Syntaxes
{

    [TestMethod]
    [DataRow("(syntax? (quote-syntax x))", "#t")]
    [DataRow("(syntax? 1)", "#f")]
    [DataRow("(syntax? (syntax->list (quote-syntax (+ 1 2 3))))", "#f")]
    [DataRow("(syntax? (car (syntax->list (quote-syntax (+ 1 2 3)))))", "#t")]
    public void SyntaxP(string input, string expected)
    {
        var actual = Utilities.InterpretUsingReadSyntax(input);
        Assert.AreEqual(actual, expected);

    }

    [TestMethod]
    [DataRow("(syntax? (syntax-e (quote-syntax boo)))", "#f")]
    [DataRow("(symbol? (syntax-e (quote-syntax boo)))", "#t")]
    [DataRow("(symbol=? 'boo (syntax-e (quote-syntax boo)))", "#t")]
    public void SyntaxE(string input, string expected)
    {
        var actual = Utilities.InterpretUsingReadSyntax(input);
        Assert.AreEqual(actual, expected);

    }

    [TestMethod]
    [DataRow("(syntax? (datum->syntax (quote-syntax b) 1))", "#t")]
    [DataRow("(syntax? (datum->syntax (quote-syntax b) '(+ 1 2 3 4)))", "#t")]
    [DataRow("(syntax? (car (syntax->list (datum->syntax (quote-syntax b) '(+ 1 2 3 4)))))", "#t")]
    public void DatumToSyntax(string input, string expected)
    {
        var actual = Utilities.InterpretUsingReadSyntax(input);
        Assert.AreEqual(actual, expected);

    }

    public void DatumToSyntaxWithQuotedListReturnsSyntax() {
        var actual = Utilities.InterpretUsingReadSyntax("(datum->syntax (quote-syntax b) '(+ 1 2 3))");
        Assert.IsInstanceOfType(actual, typeof(Jig.Syntax));

    }

}
