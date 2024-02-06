namespace JigTests;

[TestClass]
public class Macros {


    [TestMethod]
    [DataRow("(or #f 26)", "26")]
    [DataRow("(or)", "#f")]
    [DataRow("(or 1)", "1")]
    [DataRow("(or 1 2 3)", "1")]
    [DataRow("(or #f #f #f 3)", "3")]
    public void ApplyOrMacro(string input, string expected) {
        var actual = Utilities.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }


}
