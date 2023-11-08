namespace Jig.Reader;

public class Token {

    public static Token EOF = new EOFTokenType();

    public class Comment : TokenBase {
        public Comment(string text, string src, int line, int column, int start, int end) : base(text, src, line, column, start, end) {}
    }

    public class Identifier : TokenBase {

        public Identifier(string text, string src, int line, int column, int start, int end) : base(text, src, line, column, start, end) {}
    }

    public class OpenParen : TokenBase {

        public OpenParen(string src, int line, int column, int start, int end) : base("(", src, line, column, start, end) {}

    }

    public class CloseParen : TokenBase {

        public CloseParen(string src, int line, int column, int start, int end) : base(")", src, line, column, start, end) {}

    }

    public class Dot : TokenBase {
        public Dot(string src, int line, int column, int start, int end) : base(".", src, line, column, start, end) {}

    }

    public class EOFTokenType : Token {}
}


public class TokenBase : Token {

    public TokenBase(string text, string src, int line, int col, int start, int end) {
        Text = text;
        SrcLoc = new SrcLoc(src, line, col, start, end - start);
    }

    public SrcLoc SrcLoc {get;}

    public string Text;


}
