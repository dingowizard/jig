namespace Tests.Common;

    
[TestClass]
public abstract class SetsBase {
    
    protected abstract IInterpreter Interp { get; set; }
    
    [TestMethod]
    [DataRow(new string[]{"(define q 11)", "(set! q 12)", "q"}, "12")]
    [DataRow(new string[]{"(define q 11)", "(set! q (+ q 1))", "q"}, "12")]
    public void SetTopLevelVar(string[] exprs, string expected) {
        var actual = Interp.InterpretSequenceReadSyntax(exprs);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("((lambda () (define z 25) (set! z 26) z))", "26")]
    [DataRow("((lambda () (define z 25) (set! z (car (cons (+ z 1) z))) z))", "26")]
    public void SetLexicalVar(string input, string expected) {
        var actual = Interp.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }


    
}