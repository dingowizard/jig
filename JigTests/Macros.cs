namespace JigTests;

[TestClass]
public class Macros {


    // [TestMethod]
    // [DataRow("(or #f 26)", "26")]
    // [DataRow("(or)", "#f")]
    // [DataRow("(or 1)", "1")]
    // [DataRow("(or 1 2 3)", "1")]
    // [DataRow("(or #f #f #f 3)", "3")]
    // public void ApplyOrMacro(string input, string expected) {
    //     var actual = Utilities.InterpretUsingReadSyntax(input);
    //     Assert.AreEqual(expected, actual);
    // }

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

}
