namespace JigTests;

[TestClass]
public class Begin
{
    [TestMethod]
    [DataRow("(begin 3 2 1)", "1")]
    public void Begins(string input, string expected)
    {
        var actual = Utilities.PreludeInterp.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }


}
