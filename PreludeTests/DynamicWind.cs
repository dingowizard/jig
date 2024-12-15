
namespace PreludeTests;

[TestClass]
public class DynamicWind
{

    [TestMethod]
    public void ReturnsInThunkDoesBeforeAndAfterThunks() {
        var actual = Utilities.PreludeInterp.InterpretSequenceReadSyntax([
            "(define b 2)",
                "(define in (dynamic-wind (lambda () (set! b (+ b 1))) (lambda () b) (lambda () (set! b (+ b 1)))))",
                "(+ in b)"
        ]);
        Assert.AreEqual("7", actual);
    }

    [TestMethod]
    public void MakeAndUseParameters() {
        var actual = Utilities.PreludeInterp.InterpretSequenceReadSyntax([
            "(define p (make-parameter 1))",
                "(define q (make-parameter 2))",
                "(+ (p) (q))"
        ]);
        Assert.AreEqual("3", actual);
    }
    
    [TestMethod]
    public void Parameterize() {
        var actual = Utilities.PreludeInterp.InterpretSequenceReadSyntax([
            "(define p (make-parameter 1))",
            "(define q (make-parameter 2))",
            "(cons (parameterize ((p 10) (q 20)) (+ (p) (q))) (+ (p) (q)))"
        ]);
        Assert.AreEqual("(30 . 3)", actual);
    }

    [TestMethod]
    public void ParameterizeParameterInFunction() {
        var actual = Utilities.PreludeInterp.InterpretSequenceReadSyntax([
            "(define p (make-parameter 1))",
            "(define foo (lambda () (p)))",
            "(cons (parameterize ((p 10)) (foo)) (foo))"
        ]);
        Assert.AreEqual("(10 . 1)", actual);
    }
    
}
