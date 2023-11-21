using System.Text;
using Jig.IO;

namespace Jig.Reader;

public class TokenStream {

    public TokenStream(InputPort ip) {
        Port = ip;
    }

    public Token Read() {
        if (_peeked is null) {
            return GetNextToken();
        } else {
            Token tmp = _peeked;
            _peeked = null;
            return tmp;

        }
    }

    public Token Peek() {
        if (_peeked is null) {
            _peeked = GetNextToken();
            return _peeked;
        }
        return _peeked;
    }

    public InputPort Port { get; }

    Token GetNextToken() {
        return Start(new StringBuilder());
    }

    void Consume() {
        Port.Read();
    }

    void Match(char c) {
        if (c == (char)Port.Peek()) {
            Consume();
            return;
        }
        throw new Exception($"in Match: expected {c} but got {(char)Port.Peek()}");
    }

    // **********************************************
    // ***** state machine *******************
    // **************************************

    Token Start(StringBuilder sb) {
        while (Char.IsWhiteSpace((char)Port.Peek())) {
            Port.Read();
        }
        if (Port.Peek() == -1) {
            return Token.EOF;
        }
        SrcLoc startLoc = new SrcLoc(Port.Source, Port.Line, Port.Column, Port.Position, 0);
        char peeked = (char)Port.Peek();
        switch (peeked) {
            //letter
            case char l when ((l >= 'a' && l <= 'z') || (l >= 'A' && l <= 'Z')):
            // special initial
            case '!': case '$': case '%': case '&': case '*' : case '/':
            case ':': case '<': case '=': case '>': case '?' : case '^':
            case '_': case '~':
                sb.Append((char)Port.Read());
                return Subsequent(sb, startLoc);
            case '+' : case '-':
                sb.Append((char)Port.Read());
                return InitialSign(sb, startLoc);
            case '#':
                sb.Append((char)Port.Read());
                return Hash(sb, startLoc);
            case '(':
                Port.Read();
                return new Token.OpenParen(startLoc.Source, startLoc.Line, startLoc.Column, startLoc.Position, 1);
            case ')':
                Port.Read();
                return new Token.CloseParen(startLoc.Source, startLoc.Line, startLoc.Column, startLoc.Position, 1);
            case '.':
                sb.Append((char)Port.Read());
                return Dot(sb, startLoc);
            case ';':
                sb.Append((char)Port.Read());
                return Comment(sb, startLoc);
            case >= '0' and <= '9':
                sb.Append((char)Port.Read());
                return Digit(sb, startLoc);
            default:
                throw new Exception($"TokenStream.Start: unhandled character '{peeked}'");

        }
    }

    private Token Digit(StringBuilder sb, SrcLoc startLoc) {
        if (AtTokenEnd()) {
            return new Token.Number(sb.ToString(), startLoc.Source, startLoc.Line, startLoc.Column, startLoc.Position, Port.Position - startLoc.Position);
        }
        char peeked = (char)Port.Peek();
        switch (peeked) {
            case >= '0' and <= '9':
                sb.Append((char)Port.Read());
                return Digit(sb, startLoc);
            case 'e': case 'E':
                sb.Append((char)Port.Read());
                return DigitsE(sb, startLoc);
            case '.':
                sb.Append((char)Port.Read());
                return SignThenDotThenDigit(sb, startLoc);
            default:
                throw new Exception($"TokenStream.Digit: unhandled next character '{peeked}'.");
        }

    }

    private Token DigitsE(StringBuilder sb, SrcLoc startLoc)
    {
        if (AtTokenEnd()) {
            throw new Exception($"Token.DigitsE: unexpected end of token"); // TODO: should this return a token.identifier? that's what chez and gosh do
        }
        char peeked = (char)Port.Peek();
        switch (peeked) {
            case char d when (d >= '0' && d <= '9'):
                sb.Append((char)Port.Read());
                return NumEDigits(sb, startLoc);
            case '-': case '+':
                sb.Append((char)Port.Read());
                return NumEDigits(sb, startLoc);
            default:
                throw new Exception($"TokenStream.DigitsE: unhandled next character '{peeked}'.");
        }
    }

    private Token NumEDigits(StringBuilder sb, SrcLoc startLoc)
    {
        if (AtTokenEnd()) {
            return new Token.Number(sb.ToString(), startLoc.Source, startLoc.Line, startLoc.Column, startLoc.Position, Port.Position - startLoc.Position);
        }
        char peeked = (char)Port.Peek();
        switch (peeked) {
            case >= '0' and <= '9':
                sb.Append((char)Port.Read());
                return NumEDigits(sb, startLoc);
            default:
                throw new Exception($"NumEDigits: unhandled next character '{peeked}'.");
        }
    }

    Token.Comment Comment(StringBuilder sb, SrcLoc startLoc) {
        while ((char)Port.Peek() != '\n' && Port.Peek() != -1) {
            // TODO: store comment text in token
            sb.Append((char)Port.Read());
        }
        return new Token.Comment(sb.ToString(), startLoc.Source, startLoc.Line, startLoc.Column, startLoc.Position, Port.Position - startLoc.Position);

    }

    private Token Dot(StringBuilder sb, SrcLoc startLoc) {
        if (AtTokenEnd()) {
            return new Token.Dot(startLoc.Source, startLoc.Line, startLoc.Column, startLoc.Position, Port.Position - startLoc.Position);
        }
        char peeked = (char)Port.Peek();
        switch (peeked) {
            case char d when (d >= '0' && d <= '9'):
                sb.Append((char)Port.Read());
                return SignThenDotThenDigit(sb, startLoc);
            case '.':
                //letter
            case char l when ((l >= 'a' && l <= 'z') || (l >= 'A' && l <= 'Z')):
                // special initial
            case '!': case '$': case '%': case '&': case '*' : case '/':
            case ':': case '<': case '=': case '>': case '?' : case '^':
            case '_': case '~':
                // sign subsequent
            case '+' : case '-':
            case '@':
                sb.Append((char)Port.Read());
                return Subsequent(sb, startLoc);
            default:
                throw new Exception($"TokenStream.Dot: unhandled next character '{peeked}'.");
        }

    }

    private Token Hash(StringBuilder sb, SrcLoc startLoc) {
        if (AtTokenEnd()) {
            throw new Exception($"syntax error: unexpected {(char)Port.Peek()} after '#'");
        }
        char peeked = (char)Port.Peek();
        switch (peeked) {
            case 't':
                sb.Append((char)Port.Read());
                return HashT(sb, startLoc);
            case 'f':
                sb.Append((char)Port.Read());
                return HashF(sb, startLoc);
            case '(':
                sb.Append((char)Port.Read());
                return HashOpen(sb, startLoc);
            case 'b': case 'B':
                sb.Append((char)Port.Read());
                return HashB(sb, startLoc);
            case 'o': case 'O':
                sb.Append((char)Port.Read());
                return HashO(sb, startLoc);
            case 'd': case 'D':
                sb.Append((char)Port.Read());
                return HashD(sb, startLoc);
            case 'x': case 'X':
                sb.Append((char)Port.Read());
                return HashX(sb, startLoc);
            case ';':
                sb.Append((char)Port.Read());
                return HashSemiColon(sb, startLoc);
            default:
                throw new Exception($"syntax error: unexpected {(char)Port.Peek()} after '#'");
        }
    }

    private Token HashSemiColon(StringBuilder sb, SrcLoc startLoc)
    {
        throw new NotImplementedException();
    }

    private Token HashX(StringBuilder sb, SrcLoc startLoc)
    {
        throw new NotImplementedException();
    }

    private Token HashD(StringBuilder sb, SrcLoc startLoc)
    {
        throw new NotImplementedException();
    }

    private Token HashO(StringBuilder sb, SrcLoc startLoc)
    {
        throw new NotImplementedException();
    }

    private Token HashB(StringBuilder sb, SrcLoc startLoc)
    {
        throw new NotImplementedException();
    }

    private Token HashOpen(StringBuilder sb, SrcLoc startLoc)
    {
        throw new NotImplementedException();
    }

    private Token HashF(StringBuilder sb, SrcLoc startLoc)
    {
        if (AtTokenEnd()) {
            return new Token.Bool(sb.ToString(), startLoc.Source, startLoc.Line, startLoc.Column, startLoc.Position, Port.Position);
        }
        char peeked = (char)Port.Peek();
        switch (peeked) {
            default:
                throw new Exception($"TokenStream.HashF: unhandled next character '{peeked}' after '#f'.");
        }
    }

    private Token HashT(StringBuilder sb, SrcLoc startLoc)
    {
        if (AtTokenEnd()) {
            return new Token.Bool(sb.ToString(), startLoc.Source, startLoc.Line, startLoc.Column, startLoc.Position, Port.Position);
        }
        char peeked = (char)Port.Peek();
        switch (peeked) {
            default:
                throw new Exception($"TokenStream.HashT: unhandled next character '{peeked}' after '#t'.");
        }
    }

    private Token InitialSign(StringBuilder sb, SrcLoc startLoc) {
        // could be + or - symbol
        if (AtTokenEnd()) {
            return new Token.Identifier(sb.ToString(), startLoc.Source, startLoc.Line, startLoc.Column, startLoc.Position, Port.Position);
        }
        char peeked = (char)Port.Peek();
        switch (peeked) {
            case >= '0' and <= '9':
            // could be start of a number
                sb.Append((char)Port.Read());
                return Digit(sb, startLoc);
            // could be start of peculiar identifier
                //letter
            case char l when ((l >= 'a' && l <= 'z') || (l >= 'A' && l <= 'Z')):
                // special initial
            case '!': case '$': case '%': case '&': case '*' : case '/':
            case ':': case '<': case '=': case '>': case '?' : case '^':
            case '_': case '~':
                // sign subsequent
            case '+' : case '-':
            case '@':
                sb.Append((char)Port.Read());
                return Subsequent(sb, startLoc);
            case '.':
                sb.Append((char)Port.Read());
                return SignThenDot(sb, startLoc);
            default:
                throw new Exception($"TokenStream.InitialSign: unhandled next character '{peeked}'.");

        }
    }

    private Token SignThenDot(StringBuilder sb, SrcLoc startLoc) {
        if (AtTokenEnd()) {
            return new Token.Identifier(sb.ToString(), startLoc.Source, startLoc.Line, startLoc.Column, startLoc.Position, Port.Position - startLoc.Position);
        }
        char peeked = (char)Port.Peek();
        switch (peeked) {
            // digit
            case char d when (d >= '0' && d <= '9'):
                sb.Append((char)Port.Read());
                return SignThenDotThenDigit(sb, startLoc);
            case '.':
                //letter
            case char l when ((l >= 'a' && l <= 'z') || (l >= 'A' && l <= 'Z')):
                // special initial
            case '!': case '$': case '%': case '&': case '*' : case '/':
            case ':': case '<': case '=': case '>': case '?' : case '^':
            case '_': case '~':
                // sign subsequent
            case '+' : case '-':
            case '@':
                sb.Append((char)Port.Read());
                return Subsequent(sb, startLoc);
            default:
                throw new Exception($"TokenStream.SignThenDot: unhandled next character '{peeked}'.");

        }

    }

    private Token SignThenDotThenDigit(StringBuilder sb, SrcLoc startLoc)
    {
        if (AtTokenEnd()) {
            return new Token.Number(sb.ToString(), startLoc.Source, startLoc.Line, startLoc.Column, startLoc.Position, Port.Position - startLoc.Position);
        }
        char peeked = (char)Port.Peek();
        switch (peeked) {
            case >= '0' and <= '9':
                sb.Append((char)Port.Read());
                // TODO: recursive calls could reassign peeked then continue?
                return SignThenDotThenDigit(sb, startLoc);
            case 'e': case 'E':
                sb.Append((char)Port.Read());
                return NumE(sb, startLoc);

            default:
                throw new Exception($"TokenStream.SignThenDotThenDigit: unhandled next character '{peeked}'.");
        }
    }

    private Token NumE(StringBuilder sb, SrcLoc startLoc)
    {
        if (AtTokenEnd()) {
            throw new Exception($"Token.DigitsE: unexpected end of token"); // TODO: should this return a token.identifier? that's what chez and gosh do
        }
        char peeked = (char)Port.Peek();
        switch (peeked) {
            case '-': case '+':
            case >= '0' and <= '9':
                sb.Append((char)Port.Read());
                return NumEDigits(sb, startLoc);
            default:
                throw new Exception($"TokenStream.NumE: unhandled next character '{peeked}'.");
        }
    }

    Token Subsequent(StringBuilder sb, SrcLoc startLoc) {
        if (AtTokenEnd()) {
            return new Token.Identifier(sb.ToString(), startLoc.Source, startLoc.Line, startLoc.Column, startLoc.Position, Port.Position - startLoc.Position);
        }
        char peeked = (char)Port.Peek();
        switch (peeked) {
            //letter
            case char l when ((l >= 'a' && l <= 'z') || (l >= 'A' && l <= 'Z')):
            // special initial
            case '!': case '$': case '%': case '&': case '*' : case '/':
            case ':': case '<': case '=': case '>': case '?' : case '^':
            case '_': case '~':
            // digit
            case char d when (d >= '0' && d <= '9'):
            // special subsequent
            case '+': case '-': case '.': case '@':
                sb.Append((char)Port.Read());
                return Subsequent(sb, startLoc);
            default:
                throw new Exception($"TokenStream.Subsequent: unhandled character '{peeked}'");

        }

    }

    private bool AtTokenEnd() {
        if (Port.Peek() == -1) return true; // EOF
        char peeked = (char)Port.Peek();
        if (Char.IsWhiteSpace(peeked)) return true;
        switch (peeked) {
            case '(': return true;
            case ')': return true;
            default: return false;
        }
    }

    Token? _peeked;

}
