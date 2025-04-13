using Jig;
using Jig.IO;
using Environment = VM.Environment;
using VM;
using Tests.Common;

namespace Tests.VM;

public class Interpreter : IInterpreter {

    public Interpreter() {
        Env = Environment.Default;
        ExEnv = Jig.ExpansionEnvironment.Default;
        Expander = new Jig.MacroExpander();
        Comp = new Compiler();
        TheVM = new Machine();
    }

    public Machine TheVM { get; set; }

    public Compiler Comp { get; set; }

    public MacroExpander Expander { get; set; }

    public ExpansionEnvironment ExEnv { get; set; }

    public Environment Env { get; set; }


    public string Interpret(string input) {
        throw new NotImplementedException();
    }
    public string InterpretUsingReadSyntax(string input) {
        Syntax? stx = Jig.Reader.Reader.ReadSyntax(InputPort.FromString(input));
        Assert.IsNotNull(stx);
        Jig.ParsedExpr program = Expander.Expand(stx, ExEnv);
        var compiler = new Compiler(); // should class be static?
        var ctEnv = new CompileTimeEnvironment(Expander.Bindings, Env);
        var code = compiler.CompileExprForREPL(program, ctEnv);
        TheVM.Load(code, Env, DoNothing);
        TheVM.Run();
        return TheVM.VAL.Print();
    }
    
    public string InterpretSequence(string[] inputs) {
        throw new NotImplementedException();
    }
    public string InterpretSequenceReadSyntax(string[] inputs) {
        Form result = Form.Void;
        foreach (var input in inputs) {
            Syntax? stx = Jig.Reader.Reader.ReadSyntax(InputPort.FromString(input));
            Assert.IsNotNull(stx);
            MacroExpander me = new MacroExpander();
            var ctEnv = new CompileTimeEnvironment(me.Bindings, Env);
            Jig.ParsedExpr program = me.Expand(stx, ExEnv);
            var compiler = new Compiler();
            var code = compiler.CompileExprForREPL(program, ctEnv);
            TheVM.Load(code, Env, DoNothing);
            TheVM.Run();
            result =  TheVM.VAL;
        }

        return result.Print();
    }

    private void DoNothing(params Form[] forms) {
        
    }
}