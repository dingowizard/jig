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
        Form result = List.Null;
        ContinuationAny cont = forms => result = forms[0];
        Machine.Load(Procedure.Template, Procedure.Environment, cont);
        Machine.Push(syntax);
        Machine.Run();
        return result as Syntax ?? throw new Exception("result of transformer should be a syntax");
    }
}
