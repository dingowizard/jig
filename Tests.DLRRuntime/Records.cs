namespace Tests.DLRRuntime;

[TestClass]
public class Records {

    [TestMethod]
    [DataRow("(record-type-descriptor? (make-record-type-descriptor 'point #f #f #f #f (vector '(mutable x) '(mutable y))))")]
    public void MakeRecordTypeDescriptorMakesRTD(string input) {
        var actual = Utilities.BareInterpretUsingReadSyntax(input);
        Assert.AreEqual("#t", actual);

    }

    [TestMethod]
    public void MakeRecordConstructorDescriptorMakesRCD() {
        string actual = new Interpreter().InterpretSequenceReadSyntax(new string[] {
                "(define rtd (make-record-type-descriptor 'point #f #f #f #f (vector '(mutable x) '(mutable y))))",
                "(record-constructor-descriptor? (make-record-constructor-descriptor rtd #f #f))",
            });
        Assert.AreEqual("#t", actual);

    }

    [TestMethod]
    public void RecordConstructorMakesRecord() {
        string actual = new Interpreter().InterpretSequenceReadSyntax(new string[] {
                "(define rtd (make-record-type-descriptor 'point #f #f #f #f (vector '(mutable x) '(mutable y))))",
                "(define rcd (make-record-constructor-descriptor rtd #f #f))",
                "(define make-point (record-constructor rcd))",
                "(record? (make-point 1 2))"
            });
        Assert.AreEqual("#t", actual);

    }

    [TestMethod]
    public void RecordPredicateMakesWorkingPredicate() {
        string actual = new Interpreter().InterpretSequenceReadSyntax(new string[] {
                "(define rtd (make-record-type-descriptor 'point #f #f #f #f (vector '(mutable x) '(mutable y))))",
                "(define rcd (make-record-constructor-descriptor rtd #f #f))",
                "(define point? (record-predicate rtd))",
                "(point? ((record-constructor rcd) 1 2))"
            });
        Assert.AreEqual("#t", actual);

    }

    [TestMethod]
    public void RecordAccessorMakesWorkingAccessors() {
        string actual = new Interpreter().InterpretSequenceReadSyntax(new string[] {
                "(define rtd (make-record-type-descriptor 'point #f #f #f #f (vector '(mutable x) '(mutable y))))",
                "(define rcd (make-record-constructor-descriptor rtd #f #f))",
                "(define point-x (record-accessor rtd 0))",
                "(define point-y (record-accessor rtd 1))",
                "(define pt ((record-constructor rcd) 1 2))",
                "(cons (point-x pt) (point-y pt))"
            });
        Assert.AreEqual("(1 . 2)", actual);

    }

    [TestMethod]
    public void ParentPredicateIsTrueForChildRecord() {
        string actual = new Interpreter().InterpretSequenceReadSyntax(new string[] {
            "(define pt2-rtd (make-record-type-descriptor 'point2d #f #f #f #f (vector '(mutable x) '(mutable y))))",
            "(define pt2-rcd (make-record-constructor-descriptor pt2-rtd #f #f))",
            "(define point2d? (record-predicate pt2-rtd))",
            "(define pt3-rtd (make-record-type-descriptor 'point3d pt2-rtd #f #f #f (vector '(mutable z))))",
            "(define pt3-rcd (make-record-constructor-descriptor pt3-rtd pt2-rcd #f))",
            "(define make-point3d (record-constructor pt3-rcd))",
            "(point2d? (make-point3d 1 2 3))",
            });
        Assert.AreEqual("#t", actual);

    }

    [TestMethod]
    public void ChildCanAccessParentFields() {
        string actual = new Interpreter().InterpretSequenceReadSyntax(new string[] {
            "(define pt2-rtd (make-record-type-descriptor 'point2d #f #f #f #f (vector '(mutable x) '(mutable y))))",
            "(define pt2-rcd (make-record-constructor-descriptor pt2-rtd #f #f))",
            "(define point2d-x (record-accessor pt2-rtd 0))",
            "(define pt3-rtd (make-record-type-descriptor 'point3d pt2-rtd #f #f #f (vector '(mutable z))))",
            "(define pt3-rcd (make-record-constructor-descriptor pt3-rtd pt2-rcd #f))",
            "(define make-point3d (record-constructor pt3-rcd))",
            "(define pt (make-point3d 1 2 3))",
            "(point2d-x pt)"
            });
        Assert.AreEqual("1", actual);

    }

    [TestMethod]
    public void ParentPredicateIsTrueForChildChildRecord() {
        string actual = new Interpreter().InterpretSequenceReadSyntax(new string[] {
            "(define pt-rtd (make-record-type-descriptor 'point #f #f #f #f (vector '(mutable x))))",
            "(define pt-rcd (make-record-constructor-descriptor pt-rtd #f #f))",
            "(define pt2-rtd (make-record-type-descriptor 'point2d pt-rtd #f #f #f (vector '(mutable y))))",
            "(define pt2-rcd (make-record-constructor-descriptor pt2-rtd pt-rcd #f))",
            "(define point? (record-predicate pt-rtd))",
            "(define pt3-rtd (make-record-type-descriptor 'point3d pt2-rtd #f #f #f (vector '(mutable z))))",
            "(define pt3-rcd (make-record-constructor-descriptor pt3-rtd pt2-rcd #f))",
            "(define make-point3d (record-constructor pt3-rcd))",
            "(point? (make-point3d 1 2 3))",
            });
        Assert.AreEqual("#t", actual);

    }

    [TestMethod]
    [DataRow("(point-x pt)", "1")]
    [DataRow("(point-y pt)", "2")]
    [DataRow("(point-z pt)", "3")]
    public void RecordAccessorCanGetGrandparentField(string last, string expected) {
        string actual = new Interpreter().InterpretSequenceReadSyntax(new string[] {
            "(define pt-rtd (make-record-type-descriptor 'point #f #f #f #f (vector '(mutable x))))",
            "(define pt-rcd (make-record-constructor-descriptor pt-rtd #f #f))",
            "(define pt2-rtd (make-record-type-descriptor 'point2d pt-rtd #f #f #f (vector '(mutable y))))",
            "(define pt2-rcd (make-record-constructor-descriptor pt2-rtd pt-rcd #f))",
            "(define pt3-rtd (make-record-type-descriptor 'point3d pt2-rtd #f #f #f (vector '(mutable z))))",
            "(define pt3-rcd (make-record-constructor-descriptor pt3-rtd pt2-rcd #f))",
            "(define make-point3d (record-constructor pt3-rcd))",
            "(define pt (make-point3d 1 2 3))",
            "(define point-x (record-accessor pt-rtd 0))",
            "(define point-y (record-accessor pt2-rtd 0))",
            "(define point-z (record-accessor pt3-rtd 0))",
            last
            });
        Assert.AreEqual(expected, actual);

    }

}