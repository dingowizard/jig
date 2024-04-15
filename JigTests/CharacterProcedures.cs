namespace JigTests;

[TestClass]
public class CharacterProcedures {

    [TestMethod]
    [DataRow("(char? #\\c)", "#t")]
    [DataRow("(char? 1)", "#f")]
    public void Char_P_RecognizesChars(string input, string expected) {
        var actual = Utilities.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }
}
