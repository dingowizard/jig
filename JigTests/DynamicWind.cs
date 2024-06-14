
namespace JigTests;

[TestClass]
public class DynamicWind
{

    [TestMethod]
    public void ReturnsInThunkDoesBeforeAndAfterThunks() {
        string actual = Utilities.PreludeInterp.InterpretSequenceReadSyntax(new string[] {
                "(define b 2)",
                "(define in (dynamic-wind (lambda () (set! b (+ b 1))) (lambda () b) (lambda () (set! b (+ b 1)))))",
                "(+ in b)"
            });
        Assert.AreEqual("7", actual);

    }
}
