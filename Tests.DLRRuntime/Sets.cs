namespace Tests.DLRRuntime.Core;

[TestClass]
public class Sets {

    [TestMethod]
    [DataRow(new string[]{"(define q 11)", "(set! q 12)", "q"}, "12")]
    public void SetTopLevelVar(string[] exprs, string expected) {
        IInterpreter interp = new Interpreter();
        string actual = "";
        foreach(string input in exprs) {
            actual = interp.Interpret(input);
        }
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("((lambda () (define z 25) (set! z 26) z))", "26")]
    [DataRow("((lambda () (define z 26) (set! z (car (cons 12 13))) z))", "12")]
    public void SetLexicalVar(string input, string expected) {
        var actual = Utilities.BareInterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow(new string[]{"(define q 11)", "(set! q 12)", "q"}, "12")]
    public void SetTopLevelVarSyntax(string[] exprs, string expected) {
        IInterpreter interp = new Interpreter();
        string actual = "";
        foreach(string input in exprs) {
            actual = interp.InterpretUsingReadSyntax(input);
        }
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("((lambda () (define z 25) (set! z 26) z))", "26")]
    [DataRow("((lambda () (define z 26) (set! z (car (cons 12 13))) z))", "12")]
    public void SetLexicalVarSyntax(string input, string expected) {
        var actual = Utilities.BareInterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

}
