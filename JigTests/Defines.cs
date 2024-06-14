namespace JigTests.Core;

[TestClass]
public class Defines {

    [TestMethod]
    [DataRow(new string[]{"(define q 12)", "q"}, "12")]
    public void DefineTopLevelVar(string[] exprs, string expected) {
        IInterpreter interp = new Interpreter(withPrelude: false);
        string actual = "";
        foreach(string input in exprs) {
            actual = interp.Interpret(input);
        }
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("((lambda () (define z (car (cons 12 13))) z))", "12")]
    [DataRow("((lambda () (define z 26) z))", "26")]
    public void DefineLexicalVar(string input, string expected) {
        var actual = Utilities.BareInterpret(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow(new string[]{"(define q 12)", "q"}, "12")]
    public void DefineTopLevelVarSyntax(string[] exprs, string expected) {
        IInterpreter interp = new Interpreter(withPrelude: false);
        string actual = "";
        foreach(string input in exprs) {
            actual = interp.InterpretUsingReadSyntax(input);
        }
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    // TODO:
    // [DataRow("(begin (define z 26) (begin (define z 1) (set! z 2)) z)", "26")]
    // actually this test is wrong.
    // in chez:
// > (begin (define z 25) (begin (define z 2)) z)
// 2
// > (begin (define z 25) ((lambda () (define z 2))) z)
// Exception: no expressions in body (lambda () (define z 2))
// Type (debug) to enter the debugger.
// > (begin (define z 25) ((lambda () (define z 2) z)) z)
// 25
    [DataRow("((lambda () (define z 26) ((lambda () (define z 2) z)) z))", "26")]
    [DataRow("((lambda () (define z 26) z))", "26")]
    [DataRow("((lambda () (define z (car (cons 12 13))) z))", "12")]
    public void DefineLexicalVarSyntax(string input, string expected) {
        var actual = Utilities.BareInterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

}
