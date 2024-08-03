namespace Jig;

public delegate Thunk? Thunk();
public delegate Thunk CompiledCode(Delegate k, IEnvironment env);

internal static class Compiler {

    public static CompiledCode Compile(Form ast) {
        var scope = new LexicalContext();
        return ET.Analyze(scope,ast).Compile();
    }

}
