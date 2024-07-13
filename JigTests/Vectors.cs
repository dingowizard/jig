namespace JigTests;

[TestClass]
public class Vectors {

    [TestMethod]
    [DataRow("(vector 1 2 3)", "#(1 2 3)")]
    public void VectorConstructor(string input, string expected) {
        var actual = Utilities.BareInterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);

    }

    [TestMethod]
    [DataRow("(vector? (vector 1 2 3))", "#t")]
    [DataRow("(vector? (list 1 2 3))", "#f")]
    [DataRow("(vector? 12)", "#f")]
    public void Vector_PKnowsWhatsAVector(string input, string expected) {
        var actual = Utilities.BareInterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);

    }

    [TestMethod]
    [DataRow("(vector-length (vector 2 4 6))", "3")]
    [DataRow("(vector-length (vector 'a 'b))", "2")]
    public void VectorLength(string input, string expected) {
        var actual = Utilities.BareInterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);

    }

    [TestMethod]
    [DataRow("(vector-ref (vector 2 4 6) 2)", "6")]
    [DataRow("(vector-ref (vector 'a 'b) 0)", "a")]
    public void VectorRef(string input, string expected) {
        var actual = Utilities.BareInterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);

    }
}
