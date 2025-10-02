using Jig;
namespace DLR;

public delegate Thunk? Thunk();
public delegate Thunk CompiledCode(Delegate k, IEnvironment env);

public static class Compiler {

    public static CompiledCode Compile(ISchemeValue ast) {
        var scope = new LexicalContext();
        return ET.Analyze(scope, ast).Compile();
    }

}
