using System.Diagnostics;
namespace Jig;

public class ConditionRTD : RecordTypeDescriptor {
    
    public static ConditionRTD Condition = new ();
    public static CompoundConditionRtd Compound = new ();
    public static MessageRtd Message = new ();
    public static WhoRtd Who = new ();
    public static IrritantsRtd Irritants = new ();
    public static SeriousConditionRtd Serious = new ();
    public static ViolationConditionRtd Violation = new ();
    public static AssertionConditionRtd Assertion = new ();
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
            if (arg is not Condition condition) {
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
            return IsOfMe(condition);
        };
    }

    public Func<Condition, SchemeValue> Accessor(Func<SchemeValue, SchemeValue> proc) {

        return (arg) => {

            if (arg is CompoundCondition compound) {
                foreach (var c in compound.SimpleConditions) {
                    Debug.Assert(c.RecordTypeDescriptor is not null);
                    if (ReferenceEquals(c.RecordTypeDescriptor, this)) {
                        // we found a simple record with the right type
                        return (SchemeValue)proc(c);
                    }
                }
                // TODO: this needs to be wrapped in something that checks to make sure the compound condtion has the right type
                throw new Exception("called accessor on value of wrong type");
            }
            return (SchemeValue)proc(arg);
        };
    }
    
    
}

public class CompoundConditionRtd : ConditionRTD {
    
    public CompoundConditionRtd() : base (new Symbol("&compound-condition"), ConditionRTD.Condition, [new Tuple<Symbol, bool>(new Symbol("simple-conditions"), false)]) {}
    
}

public class WhoRtd : ConditionRTD {
    
    public WhoRtd() : base (new Symbol("&who"), ConditionRTD.Condition, [new Tuple<Symbol, bool>(new Symbol("who"), false)]) {}
    
}

public class IrritantsRtd : ConditionRTD {
    
    public IrritantsRtd() : base (new Symbol("&irritants"), ConditionRTD.Condition, [new Tuple<Symbol, bool>(new Symbol("irritants"), false)]) {}
    
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

