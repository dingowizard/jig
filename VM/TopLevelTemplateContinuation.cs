using Jig;

namespace VM;

public class TopLevelTemplateContinuation : TopLevelContinuation {
    public TopLevelTemplateContinuation(ContinuationAny cont) : base(cont) {
    }

    public override void Pop(Machine vm) {
        
        
        vm.PC = 0;
        vm.Template = End;
    }
    
    
    private static readonly Template End = new (
        1,
        code: [
            (ulong)OpCode.Halt << 56,
        ],
        vars: [],
        lits: [],
        0,
        true
    );
    
    
    public override int Required => 0;
    public override bool HasOptional => true;

}
