namespace JigTests;

[TestClass]
public class Arithmetic
{
    [TestMethod]
    [DataRow("(+)", "0")]
    [DataRow("(+ 2)", "2")]
    [DataRow("(+ 2 3 4)", "9")]
    [DataRow("(+ 2.2 3 4)", "9.2")]
    public void Sums(string input, string expected) {
        var actual = Utilities.Interpret(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(*)", "1")]
    [DataRow("(* 4)", "4")]
    [DataRow("(* 4 2 7)", "56")]
    [DataRow("(* 7 2.5)", "17.5")]
    public void Products(string input, string expected) {
        var actual = Utilities.Interpret(input);
        Assert.AreEqual(expected, actual);
    }
}
