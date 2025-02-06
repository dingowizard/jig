using Microsoft.Scripting.Interpreter;
using Tests.Common;

namespace Tests.VM;

[TestClass]
public sealed class Lambdas : LambdasBase {
    protected override IInterpreter Interp { get; set; }

    [TestInitialize]
    public void Setup() {
        Interp = new Interpreter();
    }

}
