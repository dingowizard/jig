using Jig;

namespace VM;

public class CompileTimeEnvironment {
    
    private Dictionary<Form.Symbol, Binding> _toplevels;
    public Binding LookUpTopLevel(Form.Symbol sym) {
        if (_toplevels.TryGetValue(sym, out var value)) {
            return value;
        }
        throw new Exception($"syntax error: undeclared variable {sym.Print()}");
    }

    public CompileTimeEnvironment(Jig.Binding[] meBindings, Environment env) {
        _toplevels = env.TopLevels;
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