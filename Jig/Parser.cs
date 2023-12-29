using System.Diagnostics;

namespace Jig.Reader;

public class Parser {

    public static SyntaxObject ParseSyntax(TokenStream tokenStream) {
        return (SyntaxObject)ParseExpr(tokenStream, syntax: true);
    }

    public static Expr ParseExpr(TokenStream tokenStream, bool syntax = false) {
        var peeked = tokenStream.Peek();
        switch (peeked) {
            case Token.Quote quoteToken:
                tokenStream.Read();
                Expr arg = ParseExpr(tokenStream, syntax);
                if (syntax) {
                   return new SyntaxObject(
                       List.NewList(new SyntaxObject.Identifier(new Expr.Symbol("quote"), quoteToken.SrcLoc), arg),
                       ((SyntaxObject)arg).SrcLoc);
                } else {
                    return List.NewList(new Expr.Symbol("quote"), arg);
                }
            case Token.OpenParen openToken:
                tokenStream.Read();
                // Expr is either a pair or an empty list
                if (tokenStream.Peek() is Token.CloseParen closeToken) {
                    // this is an empty list
                    tokenStream.Read();
                    if (syntax) {
                        return new SyntaxObject(List.Empty, SrcLoc.Combine(openToken.SrcLoc, closeToken.SrcLoc));
                    } else {
                        return List.Empty;
                    }
                }
                Expr pair =  ParsePair(tokenStream, syntax);
                if (tokenStream.Peek() is Token.CloseParen close) {
                    tokenStream.Read();
                    if (syntax) {
                        return SyntaxObject.FromDatum(pair, SrcLoc.Combine(openToken.SrcLoc, close.SrcLoc));
                    } else {

                        return pair;
                    }
                } else {
                    throw new Exception($"Expected ')' but got {tokenStream.Peek()}");
                }
            case Token.Identifier id:
                return ParseSymbol(id, tokenStream, syntax);
            case Token.Number num:
                return ParseNumber(num, tokenStream, syntax);
            case Token.String str:
                return ParseString(str, tokenStream, syntax);
            case Token.Bool b:
                return ParseBool(b, tokenStream, syntax);
            case Token.EOFTokenType _:
                throw new Exception("parse error: unexpected EOF.");
            default:
                throw new Exception($"parse error: unhandled Token {peeked}");
        }
    }

    private static Expr ParseString(Token.String str, TokenStream tokenStream, bool syntax) {
        tokenStream.Read();
        var x = new Expr.String(str.Text.Substring(1, str.Text.Length - 2));
        if (syntax) {
            return new SyntaxObject(x, str.SrcLoc);
        } else {
            return x;
        }
    }

    private static Expr ParseNumber(Token.Number num, TokenStream tokenStream, bool syntax)
    {
        tokenStream.Read();
        if (Int32.TryParse(num.Text, out int i)) {
            if (syntax) {
                return new SyntaxObject.Literal(new Expr.Integer(i), num.SrcLoc);
            } else {
                return new Expr.Integer(i);
            }
        } else if (Double.TryParse(num.Text, out double d)) {
            if (syntax) {
                return new SyntaxObject.Literal(new Expr.Double(d), num.SrcLoc);
            } else {
                return new Expr.Double(d);
            }
        } else {
            throw new Exception($"ParseExpr: could not parse number token {num.Text} to number.");
        }
    }

    private static Expr ParseBool(Token tok, TokenStream tokenStream, bool syntax = false) {
        tokenStream.Read();
        switch (tok) {
            case Token.Bool b:
                // tokenStream.Read();
                if (b.Text == "#t" || b.Text == "#true") {
                    if (syntax) {
                        return new SyntaxObject.Literal(new Expr.Boolean(true), b.SrcLoc);
                    } else {
                        return new Expr.Boolean(true);
                    }
                } else if (b.Text == "#f" || b.Text == "#false") {
                    if (syntax) {
                        return new SyntaxObject.Literal(new Expr.Boolean(false), b.SrcLoc);
                    } else {
                        return new Expr.Boolean(false);
                    }
                } else {
                    throw new Exception($"ParseLiteral: couldn't match boolean token {b}");
                }
            default:
                throw new Exception($"ParseBool: unhandled case {tok}");
        }

    }

    static Expr ParseSymbol(Token.Identifier id, TokenStream tokenStream, bool syntax = false) {
        var tok = tokenStream.Read();
        Debug.Assert(tok is Token.Identifier);
        if (syntax) {
            return new SyntaxObject.Identifier(new Expr.Symbol(id.Text), id.SrcLoc);
        } else {
            return new Expr.Symbol(id.Text);
        }
    }

    static Expr ParsePair(TokenStream tokenStream, bool syntax = false) {
        Expr car = ParseExpr(tokenStream, syntax);
        // next token could be dot, closeparen or something else, in which case the rest will be another pair
        Expr cdr;
        if (tokenStream.Peek() is Token.Dot) {
            tokenStream.Read();
            cdr = ParseExpr(tokenStream, syntax);
            return (Expr)Expr.Pair.Cons(car, cdr);
        }
        if (tokenStream.Peek() is Token.CloseParen close) {
            cdr = List.Empty;
            return (Expr)Expr.Pair.Cons(car, cdr);

        }
        cdr = ParsePair(tokenStream, syntax);
        return (Expr)Expr.Pair.Cons(car, cdr);


    }

}
