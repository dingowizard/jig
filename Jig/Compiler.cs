namespace Jig;

public delegate Continuation.MaybeThunk CompiledCode(Delegate k, IEnvironment env);

internal static class Compiler {

    public static CompiledCode Compile(Expr ast) {
        var scope = new LexicalContext();
        return ET.Analyze(scope,ast).Compile();
    }

}
