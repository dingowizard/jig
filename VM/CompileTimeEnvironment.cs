using Jig;

namespace VM;

public class CompileTimeEnvironment {
    public Binding LookUpTopLevel(Form.Symbol sym) {
        throw new NotImplementedException();
    }

    public (int, int) LookUpLexVar(Form.Symbol sym) {
        throw new NotImplementedException();
    }
}