using Jig;
using Jig.IO;

namespace JigTests;

[TestClass]
public class Literals
{
    [TestMethod]
    [DataRow("1", "1")]
    [DataRow("-12", "-12")]
    [DataRow("+12", "12")]
    [DataRow("+1e2", "100")] // TODO: actualy this shouldn't pass. should make a double (need #e+1e2 for 100)
    public void EvalIntReturnsInt(string input, string expected)
    {
        var actual = Utilities.Interpret(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("\"hello\"", "\"hello\"")]
    [DataRow("\"hello goodbye hello goodbye\"", "\"hello goodbye hello goodbye\"")]
    public void EvalStringReturnsString(string input, string expected)
    {
        var actual = Utilities.Interpret(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("\"hello\"", "\"hello\"")]
    [DataRow("\"hello goodbye hello goodbye\"", "\"hello goodbye hello goodbye\"")]
    public void EvalStringReturnsStringUsingReadSyntax(string input, string expected)
    {
        var actual = Utilities.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("#\\c", "#\\c")]
    [DataRow("#\\1", "#\\1")]
    public void EvalCharReturnsCharReadSyntax(string input, string expected)
    {
        var actual = Utilities.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("1.2", "1.2")]
    public void EvalDoubleReturnsDouble(string input, string expected)
    {
        var actual = Utilities.Interpret(input);
        Assert.AreEqual(expected, actual);

    }

    [TestMethod]
    [DataRow("#f", "#f")]
    [DataRow("#t", "#t")]
    public void EvalBools(string input, string expected)
    {
        var actual = Utilities.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);

    }

    [TestMethod]
    [DataRow(new string[]{"(define a 1)", "a"}, "1")]
    [DataRow(new string[]{"(define a #t)", "a"}, "#t")]
    public void EvalVars(string[] exprs, string expected) {
        IInterpreter interp = new Interpreter();
        string actual = "";
        foreach(string input in exprs) {
            actual = interp.Interpret(input);
        }
        Assert.AreEqual(expected, actual);
    }


    [TestMethod]
    [DataRow("(quote b)", "b")]
    [DataRow("'b", "b")]
    [DataRow("(quote (1 2 3))", "(1 2 3)")]
    [DataRow("(quote (a b c))", "(a b c)")]
    [DataRow("(quote (a b (c d)  e))", "(a b (c d) e)")]
    [DataRow("'(1 2 3)", "(1 2 3)")]
    public void QuotedListsSyntax(string input, string expected) {
        var actual = Utilities.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

}
