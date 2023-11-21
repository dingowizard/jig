using Jig;
using Jig.IO;

namespace JigTests;

[TestClass]
public class Literals
{
    [TestMethod]
    public void EvalIntReturnsInt()
    {
        Expr result = List.Empty;
        Continuation setResult = (x) => result = x;
        Program.Eval(setResult, Jig.Reader.Reader.ReadSyntax(InputPort.FromString("1")), new Jig.Environment());
        Assert.AreEqual(new Expr.Integer(1), result);

    }

    [TestMethod]
    [DataRow("-12", "-12")]
    [DataRow("+12", "12")]
    [DataRow("+1e2", "100")] // TODO: actualy this shouldn't pass. should make a double (need #e+1e2 for 100)
    public void EvalPrintVariousIntegers(string input, string expected) {
        string actual = "";
        Continuation setResult = (x) => actual = x.Print();
        Program.Eval(setResult, Jig.Reader.Reader.ReadSyntax(InputPort.FromString(input)), new Jig.Environment());
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    public void EvalDoubleReturnsDouble()
    {
        Expr result = List.Empty;
        Continuation setResult = (x) => result = x;
        Program.Eval(setResult, Jig.Reader.Reader.ReadSyntax(InputPort.FromString("1.2")), new Jig.Environment());
        Assert.AreEqual(new Expr.Double(1.2), result);

    }

    [TestMethod]
    public void EvalTrueReturnsTrue()
    {
        Expr result = List.Empty;
        Continuation setResult = (x) => result = x;
        Program.Eval(setResult, Jig.Reader.Reader.ReadSyntax(InputPort.FromString("#t")), new Jig.Environment());
        Assert.AreEqual(new Expr.Boolean(true), result);

    }

    [TestMethod]
    public void EvalFalseReturnsFalse()
    {
        Expr result = List.Empty;
        Continuation setResult = (x) => result = x;
        Program.Eval(setResult, Jig.Reader.Reader.ReadSyntax(InputPort.FromString("#f")), new Jig.Environment());
        Assert.AreEqual(new Expr.Boolean(false), result);

    }

    [TestMethod]
    public void EvalPrintFalseReturnsHashF()
    {
        string result = "";
        Continuation setResult = (x) => result = x.Print();
        Program.Eval(setResult, Jig.Reader.Reader.ReadSyntax(InputPort.FromString("#f")), new Jig.Environment());
        Assert.AreEqual("#f", result);

    }

    [TestMethod]
    public void EvalPrintIntVar()
    {
        string result = "";
        Continuation setResult = (x) => result = x.Print();
        Program.Eval(setResult, Jig.Reader.Reader.ReadSyntax(InputPort.FromString("myVar")), new Jig.Environment());
        Assert.AreEqual("12", result);

    }

    [TestMethod]
    public void EvalIntVar()
    {
        Expr result = List.Empty;
        Continuation setResult = (x) => result = x;
        Program.Eval(setResult, Jig.Reader.Reader.ReadSyntax(InputPort.FromString("myVar")), new Jig.Environment());
        Assert.AreEqual(new Expr.Integer(12), result);

    }

    [TestMethod]
    public void EvalQuotedSymbol()
    {
        Expr result = List.Empty;
        Continuation setResult = (x) => result = x;
        Program.Eval(setResult, Jig.Reader.Reader.ReadSyntax(InputPort.FromString("(quote b)")), new Jig.Environment());
        Assert.AreEqual(new Expr.Symbol("b"), result);

    }
}
