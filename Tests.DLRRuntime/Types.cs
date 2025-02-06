namespace Tests.DLRRuntime;

[TestClass]
public class Types {
    
    [TestMethod]
    [DataRow("(procedure? 0)", "#f")]
    [DataRow("(procedure? #t)", "#f")]
    [DataRow("(procedure? (lambda (x) x))", "#t")]
    [DataRow("(procedure? (lambda xs xs))", "#t")]
    [DataRow("(procedure? car)", "#t")]
    public void Procedure_P(string input, string expected)
    {
        var actual = Utilities.BareInterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);

    }
    [TestMethod]
    [DataRow("(boolean? 0)", "#f")]
    [DataRow("(boolean? #t)", "#t")]
    [DataRow("(boolean? (lambda (x) x))", "#f")]
    [DataRow("(boolean? #\\a)", "#f")]
    [DataRow("(boolean? \"car\")", "#f")]
    public void Boolean_P(string input, string expected)
    {
        var actual = Utilities.BareInterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);

    }
    
    [TestMethod]
    [DataRow("(string? 0)", "#f")]
    [DataRow("(string? #t)", "#f")]
    [DataRow("(string? (lambda (x) x))", "#f")]
    [DataRow("(string? #\\a)", "#f")]
    [DataRow("(string? \"car\")", "#t")]
    public void Sting_P(string input, string expected)
    {
        var actual = Utilities.BareInterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);

    }
    
    [TestMethod]
    [DataRow("(char? #\\c)", "#t")]
    [DataRow("(char? 1)", "#f")]
    [DataRow("(char? #t)", "#f")]
    [DataRow("(char? (lambda (x) x))", "#f")]
    [DataRow("(char? \"car\")", "#f")]
    public void Char_P_RecognizesChars(string input, string expected) {
        var actual = Utilities.BareInterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }
}