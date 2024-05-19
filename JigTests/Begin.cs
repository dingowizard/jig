namespace JigTests;

[TestClass]
public class Begin
{
    static IInterpreter _interp;

    [ClassInitialize]
    public static void MakeInterp() {
        _interp = new Interpreter(withPrelude: true);
    }

    [TestMethod]
    [DataRow("(begin 3 2 1)", "1")]
    public void Begins(string input, string expected)
    {
        var actual = _interp.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }


}
