using Jig;

namespace VM;

public class TopLevelContinuation : Continuation {
    public TopLevelContinuation(ContinuationAny cont) {
        Procedure = cont;
    }

    public override void Pop(Machine vm) {
        // Console.WriteLine("Toplevel!");
        var results = new System.Collections.Generic.List<SchemeValue>();
        while (vm.SP > 0) {
            results.Add(vm.Pop());
        }

        Procedure(results.ToArray());
    }
    
    public ContinuationAny Procedure { get; }
    
    public override int Required => 0;
    public override bool HasOptional => true;

}