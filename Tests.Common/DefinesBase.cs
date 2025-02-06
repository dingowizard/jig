namespace Tests.Common;

    
[TestClass]
public abstract class DefinesBase {
    
    protected abstract IInterpreter Interp { get; set; }
    

    [TestMethod]
    [DataRow(new string[]{"(define q 12)", "q"}, "12")]
    public void DefineTopLevelVarSyntax(string[] exprs, string expected) {
        var actual = Interp.InterpretSequenceReadSyntax(exprs);
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
        var actual = Interp.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }
}