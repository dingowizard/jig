namespace Jig.Reader;

public class Token {


    public static Token EOF = new EOFTokenType();

    public class Comment : TokenBase {
        public Comment(string text, string src, int line, int column, int start, int span) : base(text, src, line, column, start, span) {}
    }

    public class Identifier : TokenBase {

        public Identifier(string text, string src, int line, int column, int start, int span) : base(text, src, line, column, start, span) {}
    }

    public class OpenParen : TokenBase {

        public OpenParen(string src, int line, int column, int start, int span) : base("(", src, line, column, start, span) {}

    }

    public class Quote : TokenBase {

        public Quote(string src, int line, int column, int start, int span) : base("'", src, line, column, start, span) {}

    }

    public class CloseParen : TokenBase {

        public CloseParen(string src, int line, int column, int start, int span) : base(")", src, line, column, start, span) {}

    }

    public class Dot : TokenBase {
        public Dot(string src, int line, int column, int start, int span) : base(".", src, line, column, start, span) {}

    }

    public class Bool : TokenBase {
        public Bool(string text, string src, int line, int column, int start, int span) : base (text, src, line, column, start, span) {}
    }

    public class EOFTokenType : Token {}

    public class Number : TokenBase
    {
        public Number(string text, string source, int line, int col, int start, int span) : base(text, source, line, col, start, span)
        {
        }
    }
}


public class TokenBase : Token {

    public TokenBase(string text, string src, int line, int col, int start, int span) {
        Text = text;
        SrcLoc = new SrcLoc(src, line, col, start, span);
    }

    public SrcLoc SrcLoc {get;}
    public string Text;


}
