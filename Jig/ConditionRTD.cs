using System.Diagnostics;
namespace Jig;

public class ConditionRTD : RecordTypeDescriptor {
    
    public static ConditionRTD Condition = new ConditionRTD();
    public static CompoundConditionRtd Compound = new CompoundConditionRtd();
    public static MessageRtd Message = new MessageRtd();
    public static SeriousConditionRtd Serious = new SeriousConditionRtd();
    public static ViolationConditionRtd Violation = new ViolationConditionRtd();
    public static AssertionConditionRtd Assertion = new AssertionConditionRtd();
    public ConditionRTD()
        : base(
            List.NewList(
                new Symbol("&condition"),
                Base, // parent rtd. TODO: is this the right parent-rtd for &condition?
                Bool.False, // uid
                Bool.False, // sealed
                Bool.False, // opaque
                new Vector()
            )
        )
    {
        // Per r6rs std:
        // The &condition type has no fields and is neither sealed nor opaque
    }
    
    public ConditionRTD(Symbol name, RecordTypeDescriptor parent, Tuple<Symbol, bool>[] fields) : base (name, parent, fields) {}
    
    public ConditionRTD(string name, RecordTypeDescriptor parent)
        : base (List.NewList(new Symbol(name), parent, Bool.False, Bool.False, Bool.False, new Vector())) {}
    
    public override Func<SchemeValue, Bool> Predicate() {
        
        return (arg) => {
            if (arg is not Record record) {
                return Bool.False;
            }
            if (arg is CompoundCondition compound) {
                foreach (var c in compound.SimpleConditions) {
                    Debug.Assert(c.RecordTypeDescriptor is not null);
                    if (c.RecordTypeDescriptor.IsOfMe(c).Equals(Bool.True)) {
                        // TODO: could this possibly be null? why?
                        return Bool.True;
                    }
                }
                return Bool.False;
            }
            return IsOfMe(record);
        };
    }
}

public class CompoundConditionRtd : ConditionRTD {
    
    public CompoundConditionRtd() : base (new Symbol("&compound-condition"), ConditionRTD.Condition, [new Tuple<Symbol, bool>(new Symbol("simple-conditions"), false)]) {}
    
}


public class MessageRtd : ConditionRTD {
    public MessageRtd() 
        : base(new Symbol("&message"), ConditionRTD.Condition, [new Tuple<Symbol, bool>(new Symbol("message"), false)]) {}
}

public class SeriousConditionRtd : ConditionRTD {
    public SeriousConditionRtd()
        : base( "&serious",
                Condition
            ) {}
    public SeriousConditionRtd(string name, RecordTypeDescriptor parent)
        : base( name,
            parent
        ) {}
}

public class ViolationConditionRtd : SeriousConditionRtd {
    
    public ViolationConditionRtd()
        : base( "&violation",
            Serious
        ) {}
    
    public ViolationConditionRtd(string name, RecordTypeDescriptor parent)
        : base( name,
            parent
        ) {}
    
}

public class AssertionConditionRtd : ViolationConditionRtd {
    
    public AssertionConditionRtd()
        : base( "&assertion",
            Violation
        ) {}
    
    public AssertionConditionRtd(string name, RecordTypeDescriptor parent)
        : base(name,
            parent
        ) {}
    
}

