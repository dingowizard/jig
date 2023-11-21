using Jig;
using Jig.IO;

namespace JigTests;

[TestClass]
public class ProcCalls
{

    [TestMethod]
    public void EvalProcCall()
    {
        Expr result = List.Empty;
        Continuation setResult = (x) => result = x;
        Program.Eval(setResult, Jig.Reader.Reader.Read(InputPort.FromString("(succ 0)")), new Jig.Environment());
        Assert.AreEqual(new Expr.Integer(1), result);

    }
}
