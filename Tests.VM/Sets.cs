using Tests.Common;

namespace Tests.VM;
[TestClass]
public sealed class Sets : SetsBase {
    protected override IInterpreter Interp { get; set; }

    [TestInitialize]
    public void Setup() {
        Interp = new Interpreter();
    }
    
}