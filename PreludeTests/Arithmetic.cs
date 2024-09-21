
namespace PreludeTests;

[TestClass]
public class Arithmetic
{
    [TestMethod]
    [DataRow("(zero? 0)", "#t")]
    [DataRow("(zero? 1)", "#f")]
    [DataRow("(zero? 'zero)", "#f")]
    [DataRow("(zero? '())", "#f")]
    [DataRow("(zero? 0.0)", "#t")]
    public void ZeroP(string input, string expected) {
        var actual = Utilities.PreludeInterp.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }
}