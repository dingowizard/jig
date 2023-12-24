using System.Diagnostics;
using Jig;
using Jig.IO;

namespace JigTests;

[TestClass]
public class Continuations
{

    [TestMethod]
    [DataRow("(+ 1 (call/cc (lambda (k) 2)) 3)", "6")]
    public void CallCCReturnsLastExprInLambda(string input, string expected) {
        var actual = Utilities.Interpret(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    public void ApplyingContinuation() {
        IEnvironment env = new Jig.Environment();
        string result1 = "";
        Continuation.OneArgDelegate setResult1 = (x) => result1 = x.Print();
        Expr? x = Jig.Reader.Reader.ReadSyntax(InputPort.FromString("(define cont #f)"));
        Assert.IsNotNull(x);
        Program.Eval(setResult1, x, env);
        string result2 = "";
        Continuation.OneArgDelegate setResult2 = (x) => result2 = x.Print();
        x = Jig.Reader.Reader.ReadSyntax(InputPort.FromString("(+ 1 (call/cc (lambda (k) (set! cont k) 2)) 3)"));
        Assert.IsNotNull(x);
        Program.Eval(setResult2, x, env);
        Assert.AreEqual("6", result2);
        string result3 = "";
        Continuation.OneArgDelegate setResult3 = (x) => result3 = x.Print();
        x = Jig.Reader.Reader.ReadSyntax(InputPort.FromString("(cont 3)"));
        Assert.IsNotNull(x);
        Program.Eval(setResult3, x, env); // NOTE: setResult2 is the continuation that sets the result!!
        Assert.AreEqual("7", result2);
    }

    [TestMethod]
    public void UsingApplyOnSavedContinuation() {
        string actual = new Interpreter().InterpretSequenceReadSyntax(new string[] {
                "(define cont #f)",
                "(+ 1 (call/cc (lambda (cc) (set! cont cc) 0)))",
                "(apply cont '(1))"
            });
        Assert.AreEqual("2", actual);

    }

    [TestMethod]
    public void ApplyingContinuationInIf() {
        string actual = new Interpreter().InterpretSequenceReadSyntax(new string[] {
                "(define cont #f)",
                "(if (call/cc (lambda (k) (set! cont k) #f)) 1 0)",
                "(cont (= 1 1))"
            });
        Assert.AreEqual("1", actual);
    }



}
