
namespace PreludeTests;

[TestClass]
public class Logic {
    [TestMethod]
    [DataRow("(not 1)", "#f")]
    [DataRow("(not #f)", "#t")]
    [DataRow("(not #t)", "#f")]
    [DataRow("(not 'boo)", "#f")]
    [DataRow("(not \"string\")", "#f")]
    public void Not(string input, string expected) {
        var actual = Utilities.PreludeInterp.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(or 1 2)", "1")]
    [DataRow("(or #f 2)", "2")]
    [DataRow("(or #f #f)", "#f")]
    public void ApplyOrMacro(string input, string expected) {
        var actual = Utilities.PreludeInterp.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(or 1 (oops!))", "1")]
    public void OrShortCircuits(string input, string expected) {
        var actual = Utilities.PreludeInterp.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

}