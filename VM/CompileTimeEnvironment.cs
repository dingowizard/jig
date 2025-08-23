using Jig;

namespace VM;

public class CompileTimeEnvironment {
    
    // TODO: should this be a dictionary to IRuntimeBindings?
    private readonly Dictionary<Form.Symbol, Binding> _toplevels;
    public Binding LookUpTopLevel(Form.Symbol sym) {
        if (_toplevels.TryGetValue(sym, out var value)) {
            return value;
        }
        throw new Exception($"syntax error: undeclared variable {sym.Print()}");
    }

    public CompileTimeEnvironment(IRuntimeEnvironment env) {

        if (env is null) throw new ArgumentNullException(nameof(env));
        VM.Environment vmEnv = env as VM.Environment ?? throw new Exception($"expected VM.Environment but got {env.GetType()}");
        _toplevels = vmEnv.TopLevels;
    }
    


    public Binding DefineTopLevel(Form.Symbol identifierSymbol) {
        if (_toplevels.TryGetValue(identifierSymbol, out var value)) {
            return value;
        }
        var binding = new Binding(identifierSymbol, true);
        _toplevels.Add(identifierSymbol, binding);
        return binding;
        
    }
}