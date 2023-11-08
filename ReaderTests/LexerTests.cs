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
}
