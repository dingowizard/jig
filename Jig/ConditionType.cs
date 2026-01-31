namespace Jig;

public class ConditionType : RecordTypeDescriptor {
    
    public static ConditionType Condition = new ConditionType();
    public static SeriousConditionType Serious = new SeriousConditionType();
    public static ViolationConditionType Violation = new ViolationConditionType();
    public static AssertionConditionType Assertion = new AssertionConditionType();
    public ConditionType()
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
    
    public ConditionType(string name, RecordTypeDescriptor parent)
        : base (List.NewList(new Symbol(name), parent, Bool.False, Bool.False, Bool.False, new Vector())) {}
}

public class SeriousConditionType : ConditionType {
    public SeriousConditionType()
        : base( "&serious",
                Condition
            ) {}
    public SeriousConditionType(string name, RecordTypeDescriptor parent)
        : base( name,
            parent
        ) {}
}

public class ViolationConditionType : SeriousConditionType {
    
    public ViolationConditionType()
        : base( "&violation",
            Serious
        ) {}
    
    public ViolationConditionType(string name, RecordTypeDescriptor parent)
        : base( name,
            parent
        ) {}
    
}

public class AssertionConditionType : ViolationConditionType {
    
    public AssertionConditionType()
        : base( "&assertion",
            Violation
        ) {}
    
    public AssertionConditionType(string name, RecordTypeDescriptor parent)
        : base(name,
            parent
        ) {}
    
}

