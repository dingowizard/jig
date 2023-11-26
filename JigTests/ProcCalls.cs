namespace JigTests;

[TestClass]
public class ProcCalls
{

    [TestMethod]
    [DataRow("(succ 0)", "1")]
    public void EvalProcCall(string input, string expected)
    {
        var actual = Utilities.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);

    }
}
