using Jig;

namespace VM;

public class TopLevelContinuation : Continuation {
    public TopLevelContinuation(ContinuationAny cont) {
        Procedure = cont;
    }

    public override void Pop(Machine vm) {
        // Console.WriteLine("Toplevel!");
        var results = new System.Collections.Generic.List<Form>();
        while (vm.SP > 0) {
            results.Add(vm.Pop());
        }

        Procedure(results.ToArray());
    }
    
    public override int Required => 0;
    
    public ContinuationAny Procedure { get; }
    
    public override bool HasOptional => true;

}