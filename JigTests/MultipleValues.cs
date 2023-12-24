namespace JigTests;

[TestClass]
public class MultipleValues
{
    [TestMethod]
    [DataRow("(lambda () 4)", "(lambda (a) (+ a))", "4")]
    [DataRow("(lambda () (values 1 2))", "(lambda (a b) (+ a b))", "3")]
    [DataRow("(lambda () (values 1 2 3))", "(lambda (a b c) (cons a (+ b c)))", "(1 . 5)")]
    [DataRow("(lambda () (values 1 2 3))", "(lambda l (apply + l))", "6")]
    [DataRow("(lambda () (values 1 2 3 4))", "(lambda (a . rest) (cons a (apply + rest)))", "(1 . 9)")]
    [DataRow("(lambda () (values 1 2 3 4))", "(lambda (a b . rest) (cons (+ a b) (apply + rest)))", "(3 . 7)")]
    [DataRow("(lambda () (values 1 2 3 4 5))", "(lambda (a b c . rest) (cons (+ a b c) (apply + rest)))", "(6 . 9)")]
    [DataRow("(lambda () (values 1 2 3 4 5 6))", "(lambda (a b c d . rest) (cons (+ a b c d) (apply + rest)))", "(10 . 11)")]
    [DataRow("(lambda () (values 1 2 3 4 5 6 7))", "(lambda (a b c d e . rest) (cons (- (+ c d e) (+ a b)) (apply + rest)))", "(9 . 13)")]
    public void CallWithValues(string producer, string consumer, string expected)
    {
        string input = "(call-with-values " + producer + " " + consumer + ")";
        var actual = Utilities.Interpret(input);
        Assert.AreEqual(expected, actual);

    }

    [TestMethod]
    public void UseValuesToProvideSingleArgumentsToProcCall() {
        var actual = Utilities.Interpret("(+ (values 1) (values 2) (values 3))");
        Assert.AreEqual("6", actual);
    }

    [TestMethod]
    [DataRow("(values 1 2 3)", "1, 2, 3")]
    public void TopLevelCallToValues(string input, string expected ) {
        var actual = Utilities.InterpretMultipleValues(input);
        Assert.AreEqual( expected, expected);

    }

}
