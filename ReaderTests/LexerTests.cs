using Jig.IO;

namespace Jig.Reader.Tests;

[TestClass]
public class LexerTests
{
    [TestMethod]
    public void EmptyInputReturnsEOFToken() {
        var lexer = new Lexer(InputPort.FromString(""));
        var actual = lexer.GetNextToken();
        Assert.AreEqual(Token.EOF, actual);
    }

    [TestMethod]
    public void SemicolonToEndOfFileIsComment() {
        var lexer = new Lexer(InputPort.FromString("; this is a comment"));
        var actual = lexer.GetNextToken();
        Assert.IsInstanceOfType(actual, typeof(Token.Comment));
    }

    [TestMethod]
    public void ScanEmptyListReturnsOpenAndCloseParenTokens() {
        var lexer = new Lexer(InputPort.FromString("()"));
        var actual = lexer.GetNextToken();
        Assert.IsInstanceOfType(actual, typeof(Token.OpenParen));
        actual = lexer.GetNextToken();
        Assert.IsInstanceOfType(actual, typeof(Token.CloseParen));
    }

    [TestMethod]
    public void SkipWhiteSpace() {
        var lexer = new Lexer(InputPort.FromString("   \t  (   )"));
        var actual = lexer.GetNextToken();
        Assert.IsInstanceOfType(actual, typeof(Token.OpenParen));
        actual = lexer.GetNextToken();
        Assert.IsInstanceOfType(actual, typeof(Token.CloseParen));
    }

    [TestMethod]
    public void ScanLetterSequenceReturnsIdentifier() {
        var lexer = new Lexer(InputPort.FromString("abcdefg"));
        var actual = lexer.GetNextToken();
        Assert.IsInstanceOfType(actual, typeof(Token.Identifier));
    }
}
