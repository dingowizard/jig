using System.Runtime.InteropServices;
using Jig;
using Jig.IO;

namespace PreludeTests;

[TestClass]
public class ListsAndPairs
{
    [TestMethod]
    [DataRow("(list? (cons 1 2))", "#f")]
    [DataRow("(list? (cons 1 (list)))", "#t")]
    [DataRow("(list? (cons (list 1 2 3) '()))", "#t")]
    [DataRow("(list? (cons (list 1 2 3) '(1 . 2)))", "#f")]
    [DataRow("(list? (list 1 2 3 4))", "#t")]
    public void ListMakesListAndConsMight(string input, string expected) {

        string actual = Utilities.PreludeInterp.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(list)", "()")]
    [DataRow("(list 1 2 3)", "(1 2 3)")]
    public void ListConstructor(string input, string expected) {

        string actual = Utilities.PreludeInterp.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(length '())", "0")]
    [DataRow("(length '(a b c))", "3")]
    public void Length(string input, string expected) {

        string actual = Utilities.PreludeInterp.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    [DataRow("(list-tail '() 0)", "()")]
    [DataRow("(list-tail '(1 2 3) 0)", "(1 2 3)")]
    [DataRow("(list-tail '(1 2 3) 1)", "(2 3)")]
    [DataRow("(list-tail '(1 2 3) 2)", "(3)")]
    public void ListTail(string input, string expected) {

        string actual = Utilities.PreludeInterp.InterpretUsingReadSyntax(input);
        Assert.AreEqual(expected, actual);
    }
}
