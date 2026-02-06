using Jig;
namespace VM;

public class ContinuationCondition : Condition {
    
    // TODO: it's interesting that Jig doesn't have a notion of Continuations
    // maybe it should but they should be parameterized on runtime? or it should have a continuation interface?


    public ContinuationCondition(Continuation k) : base(Rtd, [k]) {
        Continuation = k;
    }
    internal ContinuationCondition(Continuation k, ActivationStack ar) : base(Rtd, [k]) {
        Continuation = k;
        ActivationStack = ar;
    }
    public Continuation Continuation {get;}
    
    internal ActivationStack? ActivationStack {get;}
    
    public static ContinuationConditionRtd Rtd = new ContinuationConditionRtd();

}
public class ContinuationConditionRtd : ConditionRTD {
    public ContinuationConditionRtd() 
        : base(new Symbol("&continuation"), ConditionRTD.Condition, [new Tuple<Symbol, bool>(new Symbol("continuation"), false)]) {}
}
