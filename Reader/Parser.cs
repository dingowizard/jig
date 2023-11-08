using System.Diagnostics;

namespace Jig.Reader;

public class Parser {

    public static Expr ParseExpr(TokenStream tokenStream) {
        var peeked = tokenStream.Peek();
        switch (peeked) {
            case Token.OpenParen _:
                tokenStream.Read();
                return ParsePair(tokenStream);
            case Token.Identifier id:
                return ParseSymbol(id, tokenStream);
            case Token.EOFTokenType _:
                throw new Exception("parse error: unexpected EOF.");
            default:
                throw new Exception($"parse error: unhandled Token {peeked}");
        }
    }

    static Expr ParseSymbol(Token.Identifier id, TokenStream tokenStream) {
        var tok = tokenStream.Read();
        Debug.Assert(tok is Token.Identifier);
        return new Expr.Symbol(id.Text);
    }

    static Expr ParsePair(TokenStream tokenStream) {
        if (tokenStream.Peek() is Token.CloseParen) {
            tokenStream.Read();
            return List.Empty;
        }
        Expr car = ParseExpr(tokenStream);
        Expr cdr;
        if (tokenStream.Peek() is Token.Dot) {
            tokenStream.Read();
            cdr = ParseExpr(tokenStream);
            if (tokenStream.Peek() is Token.CloseParen) {
                tokenStream.Read();
                switch (cdr) {
                    // TODO: move this logic inside Cons?
                    case List list:
                        return (Expr) Expr.Pair.Cons(car, list);
                    default:
                        return (Expr) Expr.Pair.Cons(car, cdr);
                }
            } else {
                throw new Exception("parse error: expected ')' but got {tokenStream.Peek()}.");
            }
        }
        cdr = ParsePair(tokenStream);
        switch (cdr) {
            case List list:
                return (Expr) Expr.Pair.Cons(car, list);
            default:
                return (Expr) Expr.Pair.Cons(car, cdr);
        }

    }

}
