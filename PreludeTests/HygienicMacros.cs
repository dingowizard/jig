namespace PreludeTests;

[TestClass]
public class HygienicMacros {

    [TestMethod]
    public void HygienicOr() {
        var actual =
            Utilities.PreludeInterp.InterpretSequenceReadSyntax(new string [] {
                    """
                    (define-syntax or2
                      (lambda (stx)
                       (datum->syntax
                         stx
                        `(let ((x ,(car (cdr (syntax->list stx)))))
                           (if x x ,(car (cdr (cdr (syntax->list stx)))))))))
                    """,
                    "(let ((x #t)) (or2 #f x))"
                });
        Assert.AreEqual("#t", actual);
    }

}
