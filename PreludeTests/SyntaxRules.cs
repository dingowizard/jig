namespace PreludeTests;

[TestClass]
public class SyntaxRules
{

    [TestMethod]
    public void Call2()
    {
        var actual = Utilities.InterpretSequenceReadSyntax(
            "(define-syntax call2 (syntax-rules () ((_ a b c) (a b c))))",
            "(call2 + 1 2)"
        );
        Assert.AreEqual("3", actual);
    }

    [TestMethod]
    [DataRow("(and2 #f #f)", "#f")]
    [DataRow("(and2 1 #f)", "#f")]
    [DataRow("(and2 1 2)", "2")]
    public void And2(string macroUse, string expected)
    {
        var actual = Utilities.InterpretSequenceReadSyntax(
            """
            (define-syntax and2
               (syntax-rules ()
                 ((_ a b) (if a b #f))))
            """,
            macroUse
        );
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    public void Hygiene()
    {
        var actual = Utilities.InterpretSequenceReadSyntax(
            """
            (define-syntax or2
               (syntax-rules ()
                 ((_ a b) ((lambda (tmp) (if tmp tmp b)) a))))
            """,
            "((lambda (tmp) (or2 #f tmp)) 1)"
        );
        Assert.AreEqual("1", actual);
        
    }

[TestMethod]
    [DataRow("(test)", "0")]
    [DataRow("(test a)", "1")]
    [DataRow("(test b c)", "2")]
    [DataRow("(test d e f)", "3")]
    public void CountArgs(string macroUse, string expected) {
        var actual = Utilities.InterpretSequenceReadSyntax(
            """
            (define-syntax test
              (syntax-rules ()
                ((test) 0)
                ((test _) 1)
                ((test _ _) 2)
                ((test _ _ _) 3)))
            """,
            macroUse
        );
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(test)", "(test)", "#t")]
    [DataRow("(test)", "(test 1)", "#f")]
    [DataRow("(test a)", "(test 1)", "#t")]
    [DataRow("(test a)", "(test 1 2)", "#f")]
    [DataRow("(test a)", "(test (4 3 2 1))", "#t")]
    [DataRow("(test a)", "(test (4 . 3))", "#t")]
    [DataRow("(test (a b) c)", "(test (1 2) 3)", "#t")]
    [DataRow("(test (a b) c)", "(test (1 2) (1 2 3 4 5))", "#t")]
    [DataRow("(test (a b) c)", "(test (1 2 3) 3)", "#f")]
    [DataRow("(test (a b) c)", "(test ((1 1) 2) 3)", "#t")]
    [DataRow("(test (a (b e) d) c)", "(test (1 (2 3) 4) 5)", "#t")]
    [DataRow("(test (a (b e) d) c)", "(test (1 (2 (1 1)) 4) 5)", "#t")]
    [DataRow("(test (a . b) (c) d)", "(test (1 . 2) (3) 4)", "#t")]
    [DataRow("(test (a . b) (c) d)", "(test (1 . (2 . 2)) (3) 4)", "#t")]
    [DataRow("(test (a . (b . e)) (c) d)", "(test (1 . (2 . 2)) (3) 4)", "#t")]
    public void CorrectConditionsForPatternVars(string pattern, string macroUse, string expected) {
        var actual = Utilities.InterpretSequenceReadSyntax(
            $"(define-syntax test (syntax-rules () ({pattern} #t) ((a ...) #f)))",
            macroUse
        );
        Assert.AreEqual(expected, actual);
    }
    
    [TestMethod]
    [DataRow("(test a ...)", "(test)", "#t")]
    [DataRow("(test a b ...)", "(test)", "#f")]
    [DataRow("(test a ...)", "(test 1)", "#t")]
    [DataRow("(test a b ...)", "(test 1)", "#t")]
    [DataRow("(test (a b) ...)", "(test (1 2) (3 4) (5 6))", "#t")]
    [DataRow("(test (a b) ...)", "(test (1 2) 3 4)", "#f")]
    [DataRow("(test (a ...) ...)", "(test (1) (2 3 4) (5 6) (7 8 9 10 11 12))", "#t")]
    [DataRow("(test (a b) ...)", "(test)", "#t")]
    public void CorrectConditionsWithEllipses(string pattern, string macroUse, string expected) {
        var actual = Utilities.InterpretSequenceReadSyntax(
            $"(define-syntax test (syntax-rules () ({pattern} #t) ((a ...) #f)))",
            macroUse
        );
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    public void Append() {
        var actual = Utilities.InterpretSequenceReadSyntax(
            """
            (define-syntax my-append
              (syntax-rules ()
                ((my-append (a ...) ...) '(a ... ...))))
            """,
            "(my-append (1 2 3) (4 5 6) (7 8 9))"
        );
        Assert.AreEqual("(1 2 3 4 5 6 7 8 9)", actual);
        
    }
    
    [TestMethod]
    [DataRow("(any-of-any)", "()")]
    [DataRow("(any-of-any 1)", "(1)")]
    [DataRow("(any-of-any 1 2)", "(1 2)")]
    [DataRow("(any-of-any 1 (2 3) 4)", "(1 (2 3) 4)")]
    public void AnyOfAny(string macroUse, string expected) {
        var actual = Utilities.InterpretSequenceReadSyntax(
            """
            (define-syntax any-of-any
              (syntax-rules ()
                ((any-of-any a ...) '(a ...))))
            """,
            macroUse
        );
        Assert.AreEqual(expected, actual);
        
    }

    [TestMethod]
    public void Let() {
        var actual = Utilities.InterpretSequenceReadSyntax(
            """
            (define-syntax lit
              (syntax-rules ()
                ((lit ((p arg) ...) body0 bodies ...) ((lambda (p ...) body0 bodies ...) arg ...))))
            """,
            "(lit ((a 1) (b 2)) (+ a b))"
        );
        Assert.AreEqual("3", actual);
        
    }
}
