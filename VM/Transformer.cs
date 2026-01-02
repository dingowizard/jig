using Jig;
namespace VM;

public class Transformer : Jig.Expansion.Transformer {
    public Transformer(Procedure proc, Machine vm) {
        Machine = vm;
        Procedure = proc;

    }
    
    private Machine Machine {get;}
    
    private Procedure Procedure {get;}
    
    public override Syntax Transform(Syntax syntax) {
        SchemeValue result = List.Null;
        ContinuationAny cont = forms => result = forms[0];
        Machine.Load(Procedure.Template, Procedure.Environment, cont);
        Machine.ENVT = Procedure.Environment.ExtendForProcCall(Procedure, [syntax]);
        // Machine.Push(syntax);
        try {

            Machine.Run();
        }
        catch {
            Console.WriteLine($"exception while expanding {Syntax.ToDatum(syntax).Print()} @ {syntax.SrcLoc}");
            throw;
        }
        return result as Syntax ?? throw new Exception("result of transformer should be a syntax");
    }
}
