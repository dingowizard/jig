using Jig.IO;

namespace Jig.Reader.Tests;

[TestClass]
public class ParserTests {

    [TestMethod]
    public void ParseOpenCloseParensReturnsNull() {
        var tokenStream = new TokenStream(InputPort.FromString("()"));
        var actual = Parser.ParseExpr(tokenStream);
        Assert.AreEqual(List.Null, actual);
    }

    [TestMethod]
    public void ParseSytaxOpenCloseParensReturnsNull() {
        var tokenStream = new TokenStream(InputPort.FromString("()"));
        Syntax? actual = Parser.ParseSyntax(tokenStream);
        Assert.IsNotNull(actual);
        SrcLoc? srcLoc = actual.SrcLoc;
        Assert.IsNotNull(srcLoc);
        Assert.AreEqual(List.Null, Syntax.ToDatum(actual));
        Assert.AreEqual(1, srcLoc?.Line);
        Assert.AreEqual(1, srcLoc?.Position);
        Assert.AreEqual(0, srcLoc?.Column);
        Assert.AreEqual(2, srcLoc?.Span);
    }

    [TestMethod]
    public void ParseOneItemList() {
        var tokenStream = new TokenStream(InputPort.FromString("(abc)"));
        var actual = Parser.ParseExpr(tokenStream);
        Assert.AreEqual(List.NewList(new Form.Symbol("abc")), actual);
    }

    [TestMethod]
    public void ParseSyntaxOneItemList() {
        var tokenStream = new TokenStream(InputPort.FromString("(abc)"));
        Syntax? stx = Parser.ParseSyntax(tokenStream);
        Assert.IsNotNull(stx);
        Assert.IsInstanceOfType(stx, typeof(Syntax));
        IForm x = Syntax.E(stx);
        Assert.IsInstanceOfType(x, typeof(IPair));
        IForm car = ((IPair)x).Car;
        Assert.IsInstanceOfType(car, typeof(Syntax));
        Syntax so = (Syntax)car;
        Assert.AreEqual(new Form.Symbol("abc"), Syntax.ToDatum(so));
    }

    [TestMethod]
    public void ParseSyntaxOneItemListCdrIsNull() {
        var tokenStream = new TokenStream(InputPort.FromString("(abc)"));
        Syntax? stx = Parser.ParseSyntax(tokenStream);
        Assert.IsNotNull(stx);
        IForm x = Syntax.E(stx);
        IForm cdr = ((IPair) x).Cdr;
        // Assert.AreEqual(List.Null, cdr);
        Assert.IsInstanceOfType(cdr, typeof(IEmptyList));
    }


    [TestMethod]
    public void ParseSyntaxListHasCorrectSrcLoc() {
        var tokenStream = new TokenStream(InputPort.FromString("(abc def)"));
        Syntax? stx = Parser.ParseSyntax(tokenStream);
        Assert.IsNotNull(stx);
        Assert.AreEqual(9, stx.SrcLoc?.Span);

    }

    [TestMethod]
    public void ParseSyntaxPairHasCorrectSrcLoc() {
        var tokenStream = new TokenStream(InputPort.FromString("(abc . def)"));
        Syntax? stx = Parser.ParseSyntax(tokenStream);
        Assert.IsNotNull(stx);
        Assert.AreEqual(11, stx.SrcLoc?.Span);

    }

    [TestMethod]
    public void ParseTwoItemList() {
        var tokenStream = new TokenStream(InputPort.FromString("(abc def)"));
        var actual = Parser.ParseExpr(tokenStream);
        Assert.AreEqual(List.NewList(new Form.Symbol("abc"), new Form.Symbol("def")), actual);
    }

    [TestMethod]
    public void ParseSyntaxTwoItemList() {
        var tokenStream = new TokenStream(InputPort.FromString("(abc def)"));
        Syntax? stx = Parser.ParseSyntax(tokenStream);
        Assert.IsNotNull(stx);
        IPair? stxPair = Syntax.E(stx) as IPair;
        Assert.IsNotNull(stxPair);
        INonEmptyList? rest = stxPair.Cdr as INonEmptyList;
        Assert.IsNotNull(rest);
        Syntax.Identifier? id = rest.Car as Syntax.Identifier;
        Assert.IsNotNull(id);
        Assert.AreEqual(new Form.Symbol("def"), id.Symbol);
    }

    [TestMethod]
    public void ParseSyntaxNestedList() {
        var tokenStream = new TokenStream(InputPort.FromString("(quote (abc def))"));
        Syntax? stx = Parser.ParseSyntax(tokenStream);
        Assert.IsNotNull(stx);
        Assert.IsInstanceOfType(stx, typeof(Syntax));
        IPair? stxPair = Syntax.E(stx) as IPair;
        Assert.IsNotNull(stxPair);
        INonEmptyList? rest = stxPair.Cdr as INonEmptyList;
        Assert.IsNotNull(rest);
        // rest should be ((abc def))
        Syntax? carCdr = rest.Car as Syntax; // should be (abc def)
        Assert.IsNotNull(carCdr);
        IPair? stxPairCarCdr = Syntax.E(carCdr) as IPair;
        Assert.IsNotNull(stxPairCarCdr);
        Syntax? abc = stxPairCarCdr.Car as Syntax;
        Assert.IsNotNull(abc);
        Assert.AreEqual(new Form.Symbol("abc"), Syntax.ToDatum(abc));
        Assert.AreEqual(List.NewList(new Form.Symbol("abc"), new Form.Symbol("def")), Syntax.ToDatum(carCdr));
    }

    [TestMethod]
    public void ParseQuotedList() {
        var tokenStream = new TokenStream(InputPort.FromString("(quote (abc def))"));
        var actual = Parser.ParseExpr(tokenStream);
        Assert.AreEqual(List.NewList(new Form.Symbol("quote"),
                                     List.NewList(new Form.Symbol("abc"),
                                                  new Form.Symbol("def"))),
                        actual);
    }

    [TestMethod]
    public void ParseFourItemList() {
        var tokenStream = new TokenStream(InputPort.FromString("(abc def ghi jkl)"));
        var actual = Parser.ParseExpr(tokenStream);
        Assert.AreEqual(List.NewList(new Form.Symbol("abc"),
                                     new Form.Symbol("def"),
                                     new Form.Symbol("ghi"),
                                     new Form.Symbol("jkl")),
                        actual);
    }

    [TestMethod]
    public void ParseListCar() {
        var tokenStream = new TokenStream(InputPort.FromString("((abc def) ghi jkl)"));
        var actual = Parser.ParseExpr(tokenStream);
        Assert.AreEqual(List.NewList(List.NewList(new Form.Symbol("abc"),
                                                  new Form.Symbol("def")),
                                     new Form.Symbol("ghi"),
                                     new Form.Symbol("jkl")),
                        actual);
    }

    [TestMethod]
    public void ParseListInList() {
        var tokenStream = new TokenStream(InputPort.FromString("(abc (def ghi) jkl)"));
        var actual = Parser.ParseExpr(tokenStream);
        Assert.AreEqual(List.NewList(new Form.Symbol("abc"),
                                     List.NewList(new Form.Symbol("def"),
                                                  new Form.Symbol("ghi")),
                                     new Form.Symbol("jkl")),
                        actual);
    }
    [TestMethod]
    public void ParsePair() {
        var tokenStream = new TokenStream(InputPort.FromString("(abc . def)"));
        var actual = Parser.ParseExpr(tokenStream);
        Assert.AreEqual(Pair.Cons(new Form.Symbol("abc"), new Form.Symbol("def")), actual);
    }

    [TestMethod]
    public void ParseImproperList() {
        var tokenStream = new TokenStream(InputPort.FromString("(abc . (def . ghi))"));
        var actual = Parser.ParseExpr(tokenStream);
        Assert.AreEqual(Pair.Cons(new Form.Symbol("abc"), (Pair) Pair.Cons(new Form.Symbol("def"), new Form.Symbol("ghi"))), actual);
    }

    [TestMethod]
    public void ParseImproperListExpressedDifferently() {
        var lexer = new TokenStream(InputPort.FromString("(abc  def . ghi)"));
        var actual = Parser.ParseExpr(lexer);
        Assert.AreEqual(Pair.Cons(new Form.Symbol("abc"), (Pair) Pair.Cons(new Form.Symbol("def"), new Form.Symbol("ghi"))), actual);
    }

    [TestMethod]
    public void ParseHashFToFalse() {
        var lexer = new TokenStream(InputPort.FromString("#f"));
        var actual = Parser.ParseExpr(lexer);
        Assert.AreEqual(Bool.False, actual);
    }

    [TestMethod]
    public void ParseHashTToFalse() {
        var lexer = new TokenStream(InputPort.FromString("#t"));
        var actual = Parser.ParseExpr(lexer);
        Assert.AreEqual(Bool.True, actual);
    }

    [TestMethod]
    public void ParseDigitsToInt() {
        var lexer = new TokenStream(InputPort.FromString("1234"));
        var actual = Parser.ParseExpr(lexer);
        Assert.AreEqual(new Integer(1234), actual);
    }

    [TestMethod]
    public void ParseDigitsDotMoreDigitsToDouble() {
        var lexer = new TokenStream(InputPort.FromString("1234.5678"));
        var actual = Parser.ParseExpr(lexer);
        Assert.AreEqual(new Float(1234.5678), actual);
    }

    [TestMethod]
    public void ParseListWithSymbolAndInt() {
        var lexer = new TokenStream(InputPort.FromString("(succ 0)"));
        var actual = Parser.ParseExpr(lexer);
        Assert.AreEqual(List.NewList(new Form.Symbol("succ"), new Integer(0)), actual);

    }

    [TestMethod]
    public void ParseQuoteTextQuoteYieldsStringExpr() {
        var lexer = new TokenStream(InputPort.FromString("\"hello goodbye\""));
        var actual = Parser.ParseExpr(lexer);
        Assert.AreEqual(new String("hello goodbye"), actual);
    }

    [TestMethod]
    public void ParseListWithSymbolAndBool() {
        var lexer = new TokenStream(InputPort.FromString("(not #f)"));
        var actual = Parser.ParseExpr(lexer);
        Assert.AreEqual(List.NewList(new Form.Symbol("not"), Bool.False), actual);

    }

    [TestMethod]
    [DataRow("`(a b c)", "(quasiquote (a b c))")]
    [DataRow("`(a ,b c)", "(quasiquote (a (unquote b) c))")]
    [DataRow("`(a ,@(list b c) d)", "(quasiquote (a (unquote-splicing (list b c)) d))")]
    public void ParseQuasiquote(string input, string expected) {
        var lexer = new TokenStream(InputPort.FromString(input));
        var actual = Parser.ParseExpr(lexer);
        Assert.IsNotNull(actual);
        Assert.AreEqual(expected, actual.Print());

    }
}
