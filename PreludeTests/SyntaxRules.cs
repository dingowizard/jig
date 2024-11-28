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
            @"(define-syntax test
                (syntax-rules ()
                  ((test) 0)
                  ((test _) 1)
                  ((test _ _) 2)
                  ((test _ _ _) 3)))",
            macroUse
        );
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(test)", "0")]
    [DataRow("(test a)", "1")]
    [DataRow("(test (a b) c)", "2")]
    [DataRow("(test (a (b e) d) c)", "3")]
    [DataRow("(test (a . b) (c) d)", "4")]
    public void CorrectConditionsForPatternVars(string macroUse, string expected) {
        var actual = Utilities.InterpretSequenceReadSyntax(
            @"(define-syntax test
                (syntax-rules ()
                  ((test) 0)
                  ((test _) 1)
                  ((test (_ _) _) 2)
                  ((test (_ (_ _) _) _) 3)
                  ((test (_ . _) (_) _) 4)))",
            macroUse
        );
        Assert.AreEqual(expected, actual);
    }
}
