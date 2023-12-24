namespace Jig;

public delegate void Continuation(Expr arg);
public delegate void ContinuationAny(params Expr[] args);
public delegate void CompiledCode(Delegate k, IEnvironment env);

internal static class Compiler {

    // public static CompiledCode Compile(SyntaxObject stx) {
    //     var lexVars = new LexicalContext();
    //     return ET.Analyze(lexVars, stx).Compile();
    // }

    public static CompiledCode Compile(Expr ast) {
        var scope = new LexicalContext();
        return ET.Analyze(scope,ast).Compile();
    }

}
