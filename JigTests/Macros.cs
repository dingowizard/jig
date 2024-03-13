namespace JigTests;

[TestClass]
public class Macros {

    [TestMethod]
    [DataRow("(and #f 26)", "#f")]
    [DataRow("(and)", "#t")]
    [DataRow("(and 1)", "1")]
    [DataRow("(and 1 2 3)", "3")]
    [DataRow("(and #f #f #f 3)", "#f")]
    [DataRow("(and 1 #f 3)", "#f")]
    [DataRow("(and #f)", "#f")]
    [DataRow("(and #t)", "#t")]
    public void ApplyAndMacro(string input, string expected) {
        var actual = Utilities.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(or 1 2)", "1")]
    [DataRow("(or #f 2)", "2")]
    [DataRow("(or #f #f)", "#f")]
    public void ApplyOrMacro(string input, string expected) {
        var actual = Utilities.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(let ((x 1)) x)", "1")]
    public void LetMacro(string input, string expected) {
        var actual = Utilities.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }
}
