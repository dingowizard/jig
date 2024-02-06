using System.Diagnostics;

namespace Jig.Reader;

public class Parser {

    public static Syntax ParseSyntax(TokenStream tokenStream) {
        return (Syntax)ParseExpr(tokenStream, syntax: true);
    }

    public static Expr ParseExpr(TokenStream tokenStream, bool syntax = false) {
        var peeked = tokenStream.Peek();
        switch (peeked) {
            case Token.Quote quoteToken:
                tokenStream.Read();
                Expr arg = ParseExpr(tokenStream, syntax);
                if (syntax) {
                   return new Syntax(List.NewList(
                       new Syntax.Identifier(Expr.Symbol.FromName("quote"), quoteToken.SrcLoc), arg),
                       ((Syntax)arg).SrcLoc);
                } else {
                    return List.NewList(Expr.Symbol.FromName("quote"), arg);
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
                        return Syntax.FromDatum(pair, SrcLoc.Combine(openToken.SrcLoc, close.SrcLoc));
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
            return new Syntax(x, str.SrcLoc);
        } else {
            return x;
        }
    }

    private static Expr ParseNumber(Token.Number num, TokenStream tokenStream, bool syntax) {
        tokenStream.Read();
        if (Int32.TryParse(num.Text, out int i)) {
            if (syntax) {
                return new Syntax.Literal(new Expr.Integer(i), num.SrcLoc);
            } else {
                return new Expr.Integer(i);
            }
        } else if (Double.TryParse(num.Text, out double d)) {
            if (syntax) {
                return new Syntax.Literal(new Expr.Double(d), num.SrcLoc);
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
                        return new Syntax.Literal(new Expr.Boolean(true), b.SrcLoc);
                    } else {
                        return new Expr.Boolean(true);
                    }
                } else if (b.Text == "#f" || b.Text == "#false") {
                    if (syntax) {
                        return new Syntax.Literal(new Expr.Boolean(false), b.SrcLoc);
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
        Expr.Symbol sym = Expr.Symbol.FromName(id.Text);
        return syntax ? new Syntax.Identifier(sym, id.SrcLoc) : sym;
    }

    static Expr ParsePair(TokenStream tokenStream, bool syntax = false) {
        Expr car = ParseExpr(tokenStream, syntax);
        // if (Keyword.Is<Keyword>(car)) {
        //     return ParseSpecialForm(car, tokenStream, syntax);
        // }
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

    private static Expr ParseSpecialForm(Expr keyword, TokenStream tokenStream, bool syntax)
    {
        if (Keyword.Is<Keyword.Lambda>(keyword)) {
            return ParseLambdaExpression(keyword, tokenStream, syntax);
        } else if (Keyword.Is<Keyword.If>(keyword)) {
            return ParseIfExpression(keyword, tokenStream, syntax);
        } else if (Keyword.Is<Keyword.Quote>(keyword)) {
            return ParseQuoteExpression(keyword, tokenStream, syntax);
        } else if (Keyword.Is<Keyword.Define>(keyword)) {
            return ParseDefineExpression(keyword, tokenStream, syntax);
        } else if (Keyword.Is<Keyword.Set>(keyword)) {
            return ParseSetExpression(keyword, tokenStream, syntax);
        }
        throw new NotImplementedException();
    }

    private static Expr ParseSetExpression(Expr keyword, TokenStream tokenStream, bool syntax)
    {
        Expr sym = ParseExpr(tokenStream, syntax);
        if (sym is not Expr.Symbol && sym is not Syntax.Identifier) {
            throw new Exception($"syntax error @ {tokenStream.Port.Source}: {tokenStream.Port.Line}, {tokenStream.Port.Column}: malformed 'set!' -- expected symbol but got {sym}.");
        }
        Expr val = ParseExpr(tokenStream, syntax);
        if (tokenStream.Peek() is not Token.CloseParen) {
            throw new Exception($"syntax error @ {tokenStream.Port.Source}: {tokenStream.Port.Line}, {tokenStream.Port.Column}: malformed 'set!' -- expected close parenthesis after value expression.");
        }
        return List.NewList(keyword, sym, val);
    }

    private static Expr ParseDefineExpression(Expr keyword, TokenStream tokenStream, bool syntax)
    {
        Expr sym = ParseExpr(tokenStream, syntax);
        if (sym is not Expr.Symbol && sym is not Syntax.Identifier) {
            throw new Exception($"syntax error @ {tokenStream.Port.Source}: {tokenStream.Port.Line}, {tokenStream.Port.Column}: malformed 'define' -- expected symbol but got {sym}.");
        }
        Expr val = ParseExpr(tokenStream, syntax);
        if (tokenStream.Peek() is not Token.CloseParen) {
            throw new Exception($"syntax error @ {tokenStream.Port.Source}: {tokenStream.Port.Line}, {tokenStream.Port.Column}: malformed 'define' -- expected close parenthesis after value expression.");
        }
        return List.NewList(keyword, sym, val);
    }

    private static Expr ParseQuoteExpression(Expr keyword, TokenStream tokenStream, bool syntax)
    {
        Expr datum = ParseExpr(tokenStream, syntax);
        // TODO: check for malformed quote, throw syntax errors
        return List.NewList(keyword, datum);
    }

    private static Expr ParseIfExpression(Expr keyword, TokenStream tokenStream, bool syntax)
    {
        if (tokenStream.Peek() is Token.CloseParen close) {
            throw new Exception($"syntax error @ {close.SrcLoc.Source}: {close.SrcLoc.Line}, {close.SrcLoc.Column}: malformed 'if' -- unexpected close parenthesis.");
        }
        Expr cond = ParseExpr(tokenStream, syntax);
        if (tokenStream.Peek() is Token.CloseParen closeTok) {
            throw new Exception($"syntax error @ {closeTok.SrcLoc.Source}: {closeTok.SrcLoc.Line}, {closeTok.SrcLoc.Column}: malformed 'if' -- unexpected close parenthesis.");
        }
        Expr conseq = ParseExpr(tokenStream, syntax);
        // TODO: handle case where if has no else branch
        Expr alt = ParseExpr(tokenStream, syntax);
        if (tokenStream.Peek() is not Token.CloseParen) {
            throw new Exception($"syntax error @ {tokenStream.Port.Source}: {tokenStream.Port.Line}, {tokenStream.Port.Column}: malformed 'if' -- expected close parenthesis.");
        }
        return List.NewList(keyword, cond, conseq, alt);
    }

    private static Expr ParseLambdaExpression(Expr keyword, TokenStream tokenStream, bool syntax)
    {
        // TODO: better error messages
        Expr parameters = ParseExpr(tokenStream, syntax);
        Console.WriteLine($"ParseLambdaExpression: parsed parameters to {parameters}");
        if (!ValidLambdaParameters(parameters)) {
            throw new Exception("syntax error: all lambda parameters must be symbols");
        }
        if (tokenStream.Peek() is Token.CloseParen) {
            throw new Exception("syntax error: lambda expressions must have a body");
        }
        List.NonEmpty body = ParseLambdaBody(tokenStream, syntax);
        return List.NewList(keyword, parameters, body);
    }

    private static List.NonEmpty ParseLambdaBody(TokenStream tokenStream, bool syntax)
    {
        if (tokenStream.Peek() is Token.Dot) {
            throw new Exception("syntax error: lambda body should be a proper list");
        }
        Expr x = ParseExpr(tokenStream, syntax);
        if (tokenStream.Peek() is Token.CloseParen) {
            return new List.NonEmpty(x, List.Empty);
        }
        if (syntax) {
            return new SyntaxList((Syntax)x, ParseLambdaBody(tokenStream, syntax));
        }
        return new List.NonEmpty(x, ParseLambdaBody(tokenStream, syntax));
    }

    private static bool ValidLambdaParameters(Expr parameters) => parameters switch
    {
        // TODO: check that no parameter names are repeated
        Syntax stx => ValidLambdaParameters(Syntax.ToDatum(stx)),
        List.NullType => true,
        Expr.Symbol => true,
        IPair pair => ValidLambdaParameters(pair.Car) && ValidLambdaParameters(pair.Cdr),
        _ => false,
    };

}
