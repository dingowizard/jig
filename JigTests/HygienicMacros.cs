namespace JigTests;

[TestClass]
public class HygienicMacros {

    [TestMethod]
    public void HygienicOr() {
        var actual =
            Utilities.InterpretUsingReadSyntax(new string [] {
                    """
                    (define-syntax or2
                      (lambda (stx)
                       (datum->syntax
                         stx
                         (list 'let
                               (list (list 'x (cadr (syntax->list stx))))
                               (list 'if 'x 'x (caddr (syntax->list stx)))))))
                    """,
                    "(let ((x 1)) (or2 #f x))"
                });
        Assert.AreEqual("1", actual);
    }

}
