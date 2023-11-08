using System.Text;
using Jig.IO;

namespace Jig.Reader;

public class TokenStream {

    public TokenStream(InputPort ip) {
        Port = ip;
        _currentToken = GetNextToken();
    }

    public Token Read() {
        Token retVal = _currentToken;
        _currentToken = GetNextToken();
        return retVal;
    }

    public Token Peek() {
        return _currentToken;
    }

    public InputPort Port { get; }

    Token GetNextToken() {
        int i = Port.Peek();
        if (i == -1) return Token.EOF;
        char c = (char)i;
        while (Char.IsWhiteSpace(c)) {// TODO: should whitespace be a token as in Racket?
            Consume();
            c = (char)Port.Peek();
        }
        switch (c) {
            case ';':
                return (Token) Comment();
            case '(':
                return (Token) OpenParen();
            case ')':
                return (Token) CloseParen();
            case '.':
                return (Token) Dot();
        }
        if (CharIsLetterOrSpecialInitial(c)) {
        // TODO: '+' and '-' should be ok as long as they are not followed by only digits. See peculiar identifier in scheme report.
            return (Token) Identifier();
        }
        throw new NotImplementedException();
    }

    bool CharIsLetterOrSpecialInitial(char c) {
        if (Char.IsLetter(c)) return true;
        var specialInitials = new char[] { '!' , '$' , '%' , '&' , '*' , '/' , ':' , '<' , '=' , '>' , '?' , '^' , '_' , '~'};
        return specialInitials.Contains(c);
    }

    Token.Identifier Identifier() {
        int start = Port.Position;
        int line = Port.Line;
        int col = Port.Column;
        StringBuilder sb = new StringBuilder();
        char init = (char)Port.Read();
        sb.Append(init);
        while (CharIsSubsequent((char)Port.Peek())) {
            sb.Append((char)Port.Read());
        }
        return new Token.Identifier(sb.ToString(), Port.Source, line, col, start, Port.Position - start);
    }

    bool CharIsSubsequent(char c) {
        if (Char.IsLetter(c)) return true;
        if (Char.IsDigit(c)) return true;
        var specials = new char[] { '!' , '$' , '%' , '&' , '*' , '/' , ':' , '<' , '=' , '>' , '?' , '^' , '_' , '~', '+', '-', '.', '@' };
        if (specials.Contains(c)) return true;
        return false;

    }

    void Consume() {
        Port.Read();
    }

    Token.OpenParen OpenParen() {
        var result = new Token.OpenParen(Port.Source, Port.Line, Port.Column, Port.Position, 1);
        Match('(');
        return result;
    }

    Token.Dot Dot() {
        var result = new Token.Dot(Port.Source, Port.Line, Port.Column, Port.Position, 1);
        Match('.');
        return result;
    }

    Token.CloseParen CloseParen() {
        var result = new Token.CloseParen(Port.Source, Port.Line, Port.Column, Port.Position, 1);
        Match(')');
        return result;
    }

    void Match(char c) {
        if (c == (char)Port.Peek()) {
            Consume();
            return;
        }
        throw new Exception($"in Match: expected {c} but got {(char)Port.Peek()}");
    }

    Token.Comment Comment() {
        int line = Port.Line;
        int column = Port.Column;
        int start = Port.Position;
        Match(';');
        var sb = new StringBuilder();
        sb.Append(';');
        while ((char)Port.Peek() != '\n' && Port.Peek() != -1) {
            // TODO: store comment text in token
            sb.Append((char)Port.Read());
        }
        return new Token.Comment(sb.ToString(), Port.Source, line, column, start, Port.Position - start);

    }

    Token _currentToken;

}

