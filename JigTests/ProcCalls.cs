namespace JigTests;

[TestClass]
public class ProcCalls
{
    [TestMethod]
    [DataRow("(procedure? 0)", "#f")]
    [DataRow("(procedure? #t)", "#f")]
    [DataRow("(procedure? (lambda (x) x))", "#t")]
    [DataRow("(procedure? (lambda xs xs))", "#t")]
    [DataRow("(procedure? car)", "#t")]
    public void Procedure_P(string input, string expected)
    {
        var actual = Utilities.BareInterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);

    }

    [TestMethod]
    [DataRow("(succ 0)", "1")]
    public void EvalProcCall(string input, string expected)
    {
        var actual = Utilities.BareInterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);

    }
}
