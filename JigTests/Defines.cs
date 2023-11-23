
using Jig;
using Jig.IO;

namespace JigTests;

[TestClass]
public class Defines {

    [TestMethod]
    public void DefineTopLevelVar() {
        string actual = "";
        Continuation setResult = (x) => actual = x.Print();
        Expr? x = Jig.Reader.Reader.Read(InputPort.FromString("(define q 12)"));
        Assert.IsNotNull(x);
        IEnvironment env = new Jig.Environment();
        Program.Eval(setResult, x, env);
        x = Jig.Reader.Reader.Read(InputPort.FromString("q"));
        Program.Eval(setResult, x, env);
        Assert.IsNotNull(x);
        Assert.AreEqual("12", actual);
    }

    [TestMethod]
    [DataRow("(begin (define z (car (cons 12 13))) z)", "12")]
    public void DefineLexicalVar(string input, string expected) {
        var actual = Utilities.Interpret(input);
        Assert.AreEqual(expected, actual);
    }

}
