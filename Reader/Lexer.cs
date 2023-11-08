using System.Text;
using Jig.IO;

namespace Jig.Reader;

public class Lexer {

    public Lexer(InputPort port) {
        Port = port;
    }

    public Lexer() {
        Port = new InputPort(Console.In);
    }

    public Token GetNextToken() {
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
        StringBuilder sb = new StringBuilder();
        char init = (char)Port.Read();
        sb.Append(init);
        while (CharIsSubsequent((char)Port.Peek())) {
            sb.Append((char)Port.Read());
        }
        return new Token.Identifier(sb.ToString(), start, Port.Position);
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
        int start = Port.Position;
        Match('(');
        return new Token.OpenParen(start, Port.Position);
    }

    Token.Dot Dot() {
        int start = Port.Position;
        Match('.');
        return new Token.Dot(start, Port.Position);
    }

    Token.CloseParen CloseParen() {
        int start = Port.Position;
        Match(')');
        return new Token.CloseParen(start, Port.Position);
    }

    void Match(char c) {
        if (c == (char)Port.Peek()) {
            Consume();
            return;
        }
        throw new Exception($"in Match: expected {c} but got {(char)Port.Peek()}");
    }



    Token.Comment Comment() {
        Match(';');
        while ((char)Port.Peek() != '\n' && Port.Peek() != -1) {
            // TODO: store comment text in token
            Consume();
        }
        return new Token.Comment();

    }

    InputPort Port {get;}

}

public class Token {

    public static Token EOF = new EOFTokenType();

    public class Comment : Token {}

    public class Identifier : TokenBase {

        public Identifier(string text, int start, int end) : base(text, start, end) {}
    }

    public class OpenParen : TokenBase {

        public OpenParen(int start, int end) : base("(", start, end) {}

    }

    public class CloseParen : TokenBase {

        public CloseParen(int start, int end) : base(")", start, end) {}

    }

    public class Dot : TokenBase {
        public Dot(int start, int end) : base(".", start, end) {}

    }

    public class EOFTokenType : Token {}
}

public class TokenBase : Token {

    public TokenBase(string text, int start, int end) {
        Text = text;
        StartPosition = start;
        EndPosition = end;
    }

    public SrcLoc SrcLoc {get;}

    public string Text;

    public int StartPosition;

    public int EndPosition;

}
