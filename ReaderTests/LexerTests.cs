using Jig.IO;

namespace Jig.Reader.Tests;

[TestClass]
public class LexerTests
{
    [TestMethod]
    public void EmptyInputReturnsEOFToken() {
        var lexer = new TokenStream(InputPort.FromString(""));
        var actual = lexer.Read();
        Assert.AreEqual(Token.EOF, actual);
    }

    [TestMethod]
    public void SemicolonToEndOfFileIsComment() {
        var lexer = new TokenStream(InputPort.FromString("; this is a comment"));
        var actual = lexer.Read();
        Assert.IsInstanceOfType(actual, typeof(Token.Comment));
    }

    [TestMethod]
    public void ScanEmptyListReturnsOpenAndCloseParenTokens() {
        var lexer = new TokenStream(InputPort.FromString("()"));
        var actual = lexer.Read();
        Assert.IsInstanceOfType(actual, typeof(Token.OpenParen));
        actual = lexer.Read();
        Assert.IsInstanceOfType(actual, typeof(Token.CloseParen));
    }

    [TestMethod]
    public void SkipWhiteSpace() {
        var lexer = new TokenStream(InputPort.FromString("   \t  (   )"));
        var actual = lexer.Read();
        Assert.IsInstanceOfType(actual, typeof(Token.OpenParen));
        actual = lexer.Read();
        Assert.IsInstanceOfType(actual, typeof(Token.CloseParen));
    }

    [TestMethod]
    public void ScanLetterSequenceReturnsIdentifier() {
        var lexer = new TokenStream(InputPort.FromString("abcdefg"));
        var actual = lexer.Read();
        Assert.IsInstanceOfType(actual, typeof(Token.Identifier));
    }
    [TestMethod]
    [DataRow("abc")]
    [DataRow("-abc")]
    [DataRow("-a2c")]
    [DataRow("-..")]
    [DataRow("-.-")]
    [DataRow(".-")]
    [DataRow(".a")]
    [DataRow("a1b2c3d4")]
    [DataRow("number->string")]
    [DataRow("null?")]
    [DataRow("-")]
    [DataRow("+")]
    [DataRow(">")]
    [DataRow("<")]
    [DataRow("$boop")]
    [DataRow("_@.")]
    public void ScanIdentifiers(string text) {
        var lexer = new TokenStream(InputPort.FromString(text));
        var actual = lexer.Read();
        Assert.IsInstanceOfType(actual, typeof(Token.Identifier));

    }

    [TestMethod]
    [DataRow("1")]
    [DataRow("1.")]
    [DataRow("12345")]
    [DataRow("-12345")]
    [DataRow(".12345")]
    [DataRow("-.12345")]
    [DataRow("+.12345")]
    [DataRow("0.12345")]
    [DataRow("-0.12345")]
    [DataRow("+0.12345")]
    [DataRow("2e3")]
    [DataRow("2E3")]
    [DataRow("-.2e3")]
    [DataRow(".2E3")]
    [DataRow("2e-3")]
    [DataRow("2.2e-3")]
    [DataRow("2.2e+3")]
    [DataRow("-2.2e+3")]
    [DataRow("-.2E3")]
    [DataRow(".2E3")]
    [DataRow("0.2E3")]
    [DataRow("-0.2E3")]
    [DataRow("+0.2E3")]
    [DataRow("-0.2E32")]
    public void ScanNumbers(string text) {
        var lexer = new TokenStream(InputPort.FromString(text));
        var actual = lexer.Read();
        Assert.IsInstanceOfType(actual, typeof(Token.Number));
    }
}
