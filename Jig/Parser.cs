using System.Diagnostics;

namespace Jig.Reader;

public class Parser {

    public static Syntax? ParseSyntax(TokenStream tokenStream) {
        return (Syntax?)ParseExpr(tokenStream, syntax: true);
    }

    public static Expr? ParseExpr(TokenStream tokenStream, bool syntax = false) {
        // TODO: handle comments!
        var peeked = tokenStream.Peek();
        while (peeked is Token.Comment) {
            tokenStream.Read();
            peeked = tokenStream.Peek();
        }
        switch (peeked) {
            case Token.Quote quoteToken:
                tokenStream.Read();
                Expr? arg = ParseExpr(tokenStream, syntax) ?? throw new Exception("unexpected EOF");
                if (syntax) {
                   return new Syntax(SyntaxList.FromParams(new Syntax.Identifier(Expr.Symbol.FromName("quote"), quoteToken.SrcLoc),
                                                           (Syntax)arg),
                       ((Syntax)arg).SrcLoc);
                } else {
                    return List.NewList(Expr.Symbol.FromName("quote"), arg);
                }
            case Token.QuasiQuote quasiquoteToken:
                tokenStream.Read();
                Expr? quasiArg = ParseExpr(tokenStream, syntax) ?? throw new Exception("unexpected EOF");
                if (syntax) {
                   return new Syntax(SyntaxList.FromParams(new Syntax.Identifier(Expr.Symbol.FromName("quasiquote"), quasiquoteToken.SrcLoc),
                                                           (Syntax)quasiArg),
                       ((Syntax)quasiArg).SrcLoc);
                } else {
                    return List.NewList(Expr.Symbol.FromName("quasiquote"), quasiArg);
                }
            case Token.UnQuote unquoteToken:
                tokenStream.Read();
                Expr? unquoteArg = ParseExpr(tokenStream, syntax) ?? throw new Exception("unexpected EOF");
                if (syntax) {
                   return new Syntax(SyntaxList.FromParams(new Syntax.Identifier(Expr.Symbol.FromName("unquote"), unquoteToken.SrcLoc),
                                                           (Syntax)unquoteArg),
                       ((Syntax)unquoteArg).SrcLoc);
                } else {
                    return List.NewList(Expr.Symbol.FromName("unquote"), unquoteArg);
                }
            case Token.UnQuoteSplicing unquoteSplicingToken:
                tokenStream.Read();
                Expr? unquoteSplicingArg = ParseExpr(tokenStream, syntax) ?? throw new Exception("unexpected EOF");
                if (syntax) {
                   return new Syntax(SyntaxList.FromParams(new Syntax.Identifier(Expr.Symbol.FromName("unquote-splicing"), unquoteSplicingToken.SrcLoc),
                                                           (Syntax)unquoteSplicingArg),
                       ((Syntax)unquoteSplicingArg).SrcLoc);
                } else {
                    return List.NewList(Expr.Symbol.FromName("unquote-splicing"), unquoteSplicingArg);
                }
            case Token.OpenParen openToken:
                tokenStream.Read();
                // Expr is either a pair or an empty list
                if (tokenStream.Peek() is Token.CloseParen closeToken) {
                    // this is an empty list
                    tokenStream.Read();
                    if (syntax) {
                        return new Syntax(List.Empty, SrcLoc.Combine(openToken.SrcLoc, closeToken.SrcLoc));
                    } else {
                        return List.Empty;
                    }
                }
                Expr pair =  ParsePair(tokenStream, syntax);
                if (tokenStream.Peek() is Token.CloseParen close) {
                    tokenStream.Read();
                    if (syntax) {
                        return new Syntax(pair, SrcLoc.Combine(openToken.SrcLoc, close.SrcLoc));
                    } else {
                        return pair;
                    }
                } else {
                    throw new Exception($"Expected ')' but got {tokenStream.Peek()}");
                }
            case Token.Identifier id:
                return ParseSymbol(id, tokenStream, syntax);
            case Token.Char c:
                return ParseChar(c, tokenStream, syntax);
            case Token.Number num:
                return ParseNumber(num, tokenStream, syntax);
            case Token.String str:
                return ParseString(str, tokenStream, syntax);
            case Token.Bool b:
                return ParseBool(b, tokenStream, syntax);
            case Token.EOFTokenType _:
                return null;
            default:
                throw new Exception($"parse error: unhandled Token {peeked}");
        }
    }

    private static Expr ParseString(Token.String str, TokenStream tokenStream, bool syntax) {
        tokenStream.Read();
        var x = new Expr.String(str.Text[1..^1]);
        if (syntax) {
            return new Syntax.Literal(x, str.SrcLoc);
        } else {
            return x;
        }
    }

    private static Expr ParseNumber(Token.Number num, TokenStream tokenStream, bool syntax) {
        tokenStream.Read();
        if (Int32.TryParse(num.Text, out int i)) {
            if (syntax) {
                return new Syntax.Literal(new Expr.IntegerNumber(i), num.SrcLoc);
            } else {
                return new Expr.IntegerNumber(i);
            }
        } else if (Double.TryParse(num.Text, out double d)) {
            if (syntax) {
                return new Syntax.Literal(new Expr.DoubleNumber(d), num.SrcLoc);
            } else {
                return new Expr.DoubleNumber(d);
            }
        } else {
            throw new Exception($"ParseExpr: could not parse number token {num.Text} to number.");
        }
    }

    private static Expr ParseChar(Token.Char cTok, TokenStream tokenStream, bool syntax = false) {
        tokenStream.Read();
        var charExpr = new Expr.Char(cTok.Text[2]);
        if (syntax) {
            return new Syntax.Literal(charExpr, cTok.SrcLoc);
        } else {
            return charExpr;
        }
    }

    private static Expr ParseBool(Token tok, TokenStream tokenStream, bool syntax = false) {
        tokenStream.Read();
        switch (tok) {
            case Token.Bool b:
                // tokenStream.Read();
                if (b.Text == "#t" || b.Text == "#true") {
                    if (syntax) {
                        return new Syntax.Literal(Expr.Bool.True, b.SrcLoc);
                    } else {
                        return Expr.Bool.True;
                    }
                } else if (b.Text == "#f" || b.Text == "#false") {
                    if (syntax) {
                        return new Syntax.Literal(Expr.Bool.False, b.SrcLoc);
                    } else {
                        return Expr.Bool.False;
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
        Expr.Symbol sym = Expr.Symbol.FromName(id.Text);
        return syntax ? new Syntax.Identifier(sym, id.SrcLoc) : sym;
    }

    static Expr ParsePair(TokenStream tokenStream, bool syntax) {
        Expr car = ParseExpr(tokenStream, syntax) ?? throw new Exception("unexpected EOF");
        if (syntax && car is not Syntax) {
            Console.WriteLine($"ParsePair: expected car to be syntax but got {car}, a {car.GetType()}");
        }
        // if (Keyword.Is<Keyword>(car)) {
        //     return ParseSpecialForm(car, tokenStream, syntax);
        // }
        // next token could be dot, closeparen or something else, in which case the rest will be another pair
        Expr? cdr;
        if (tokenStream.Peek() is Token.Dot) {
            tokenStream.Read();
            cdr = ParseExpr(tokenStream, syntax);
            if (cdr is null) throw new Exception("unexpected EOF");
            return (Expr)Expr.Pair.Cons(car, cdr);
        }

        if (tokenStream.Peek() is Token.CloseParen) {
            cdr = List.Empty;
            return (Expr)Expr.Pair.Cons(car, cdr);

        }
        cdr = ParsePair(tokenStream, syntax);
        if (cdr is null) throw new Exception("unexpected EOF");
        return (Expr)Expr.Pair.Cons(car, cdr);


    }

}
