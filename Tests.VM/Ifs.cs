using Tests.Common;

namespace Tests.VM;
[TestClass]
public sealed class Ifs : IfsBase {
    protected override IInterpreter Interp { get; set; }

    [TestInitialize]
    public void Setup() {
        Interp = new Interpreter();
    }

}
