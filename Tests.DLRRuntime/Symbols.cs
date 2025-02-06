namespace Tests.DLRRuntime.Core;

[TestClass]
public class Symbols
{

    [TestMethod]
    [DataRow("(symbol? 'boo)", "#t")]
    [DataRow("(symbol? 12)", "#f")]
    public void SymbolP(string input, string expected)
    {
        var actual = Utilities.BareInterpretUsingReadSyntax(input);
        Assert.AreEqual(actual, expected);

    }

    [TestMethod]
    [DataRow("(symbol->string 'boo)", "\"boo\"")]
    public void SymbolToString(string input, string expected)
    {
        var actual = Utilities.BareInterpretUsingReadSyntax(input);
        Assert.AreEqual(actual, expected);

    }

    [TestMethod]
    [DataRow("(symbol=? 'boo 'boo)", "#t")]
    [DataRow("(symbol=? 'boo 'bop)", "#f")]
    public void SymbolEquals(string input, string expected)
    {
        var actual = Utilities.BareInterpretUsingReadSyntax(input);
        Assert.AreEqual(actual, expected);

    }
}
