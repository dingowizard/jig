using System.Diagnostics;

namespace Jig.Reader;

public class Parser {

    public static Syntax? ParseSyntax(TokenStream tokenStream) {
        return (Syntax?)ParseExpr(tokenStream, syntax: true);
    }

    public static Form? ParseExpr(TokenStream tokenStream, bool syntax = false) {
        // TODO: handle comments!
        var peeked = tokenStream.Peek();
        while (peeked is Token.Comment) {
            tokenStream.Read();
            peeked = tokenStream.Peek();
        }
        switch (peeked) {
            case Token.Quote quoteToken:
                tokenStream.Read();
                Form? arg = ParseExpr(tokenStream, syntax) ?? throw new Exception("unexpected EOF");
                if (syntax) {
                   return new Syntax(SyntaxList.FromParams(new Syntax.Identifier(Form.Symbol.FromName("quote"), quoteToken.SrcLoc),
                                                           (Syntax)arg),
                       ((Syntax)arg).SrcLoc);
                } else {
                    return List.NewList(Form.Symbol.FromName("quote"), arg);
                }
            case Token.QuasiQuote quasiquoteToken:
                tokenStream.Read();
                Form? quasiArg = ParseExpr(tokenStream, syntax) ?? throw new Exception("unexpected EOF");
                if (syntax) {
                   return new Syntax(SyntaxList.FromParams(new Syntax.Identifier(Form.Symbol.FromName("quasiquote"), quasiquoteToken.SrcLoc),
                                                           (Syntax)quasiArg),
                       ((Syntax)quasiArg).SrcLoc);
                } else {
                    return List.NewList(Form.Symbol.FromName("quasiquote"), quasiArg);
                }
            case Token.UnQuote unquoteToken:
                tokenStream.Read();
                Form? unquoteArg = ParseExpr(tokenStream, syntax) ?? throw new Exception("unexpected EOF");
                if (syntax) {
                   return new Syntax(SyntaxList.FromParams(new Syntax.Identifier(Form.Symbol.FromName("unquote"), unquoteToken.SrcLoc),
                                                           (Syntax)unquoteArg),
                       ((Syntax)unquoteArg).SrcLoc);
                } else {
                    return List.NewList(Form.Symbol.FromName("unquote"), unquoteArg);
                }
            case Token.UnQuoteSplicing unquoteSplicingToken:
                tokenStream.Read();
                Form? unquoteSplicingArg = ParseExpr(tokenStream, syntax) ?? throw new Exception("unexpected EOF");
                if (syntax) {
                   return new Syntax(SyntaxList.FromParams(new Syntax.Identifier(Form.Symbol.FromName("unquote-splicing"), unquoteSplicingToken.SrcLoc),
                                                           (Syntax)unquoteSplicingArg),
                       ((Syntax)unquoteSplicingArg).SrcLoc);
                } else {
                    return List.NewList(Form.Symbol.FromName("unquote-splicing"), unquoteSplicingArg);
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
                Form pair =  ParsePair(tokenStream, syntax);
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

    private static Form ParseString(Token.String str, TokenStream tokenStream, bool syntax) {
        tokenStream.Read();
        var x = new Form.String(str.Text[1..^1]);
        if (syntax) {
            return new Syntax.Literal(x, str.SrcLoc);
        } else {
            return x;
        }
    }

    private static Form ParseNumber(Token.Number num, TokenStream tokenStream, bool syntax) {
        tokenStream.Read();
        if (Int32.TryParse(num.Text, out int i)) {
            if (syntax) {
                return new Syntax.Literal(new Form.IntegerNumber(i), num.SrcLoc);
            } else {
                return new Form.IntegerNumber(i);
            }
        } else if (Double.TryParse(num.Text, out double d)) {
            if (syntax) {
                return new Syntax.Literal(new Form.DoubleNumber(d), num.SrcLoc);
            } else {
                return new Form.DoubleNumber(d);
            }
        } else {
            throw new Exception($"ParseExpr: could not parse number token {num.Text} to number.");
        }
    }

    private static Form ParseChar(Token.Char cTok, TokenStream tokenStream, bool syntax = false) {
        tokenStream.Read();
        var charExpr = new Form.Char(cTok.Text[2]);
        if (syntax) {
            return new Syntax.Literal(charExpr, cTok.SrcLoc);
        } else {
            return charExpr;
        }
    }

    private static Form ParseBool(Token tok, TokenStream tokenStream, bool syntax = false) {
        tokenStream.Read();
        switch (tok) {
            case Token.Bool b:
                // tokenStream.Read();
                if (b.Text == "#t" || b.Text == "#true") {
                    if (syntax) {
                        return new Syntax.Literal(Form.Bool.True, b.SrcLoc);
                    } else {
                        return Form.Bool.True;
                    }
                } else if (b.Text == "#f" || b.Text == "#false") {
                    if (syntax) {
                        return new Syntax.Literal(Form.Bool.False, b.SrcLoc);
                    } else {
                        return Form.Bool.False;
                    }
                } else {
                    throw new Exception($"ParseLiteral: couldn't match boolean token {b}");
                }
            default:
                throw new Exception($"ParseBool: unhandled case {tok}");
        }

    }

    static Form ParseSymbol(Token.Identifier id, TokenStream tokenStream, bool syntax = false) {
        var tok = tokenStream.Read();
        Debug.Assert(tok is Token.Identifier);
        Form.Symbol sym = Form.Symbol.FromName(id.Text);
        return syntax ? new Syntax.Identifier(sym, id.SrcLoc) : sym;
    }

    static Form ParsePair(TokenStream tokenStream, bool syntax) {
        Form car = ParseExpr(tokenStream, syntax) ?? throw new Exception("unexpected EOF");
        if (syntax && car is not Syntax) {
            Console.WriteLine($"ParsePair: expected car to be syntax but got {car}, a {car.GetType()}");
        }
        // if (Keyword.Is<Keyword>(car)) {
        //     return ParseSpecialForm(car, tokenStream, syntax);
        // }
        // next token could be dot, closeparen or something else, in which case the rest will be another pair
        Form? cdr;
        if (tokenStream.Peek() is Token.Dot) {
            tokenStream.Read();
            cdr = ParseExpr(tokenStream, syntax);
            if (cdr is null) throw new Exception("unexpected EOF");
            return (Form)Form.Pair.Cons(car, cdr);
        }

        if (tokenStream.Peek() is Token.CloseParen) {
            cdr = List.Empty;
            return (Form)Form.Pair.Cons(car, cdr);

        }
        cdr = ParsePair(tokenStream, syntax);
        if (cdr is null) throw new Exception("unexpected EOF");
        return (Form)Form.Pair.Cons(car, cdr);


    }

}
