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
        var actual = Utilities.BareInterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(and 1 #f (oops!))", "#f")]
    public void AndShortCircuits(string input, string expected) {
        var actual = Utilities.BareInterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }


}
