namespace Jig;

public class Condition : Record {
    
    public static Condition Make(ConditionRTD rtd, ConstructorDescriptor rcd, List args) {
        // TODO: Maybe this should be on ConstructorDescriptor. ParameterCount logic is repeated there
        if (rcd.ParentRCD is null) {
            return new Condition(rtd, args);
        }
        int ownArgNum = rtd.Fields.Length;
        int parentArgNum = ParameterCount(rcd.ParentRCD, rtd) - ownArgNum;
        return new Condition(
            rtd,
            Make(rcd.ParentRCD.RTD, rcd.ParentRCD, args.Take(parentArgNum).ToJigList()),
            args.Skip(parentArgNum).ToJigList());
            
    }
    
    public Condition(ConditionRTD rtd, IEnumerable<ISchemeValue> fields) : base(rtd, fields) {
        ConditionRtd = rtd;
    }

    public Condition(ConditionRTD rtd, Record parent, List fields) : base(rtd, parent, fields) {
        ConditionRtd = rtd;
    }
    
    public ConditionRTD ConditionRtd { get; }
    
}

public class CompoundCondition : Condition {

    public static CompoundCondition Make(params Condition[] conditions) {
        
        var flattenedList = Flatten(conditions).ToArray();
        return new CompoundCondition(ConditionRTD.Compound, flattenedList);

    }

    private static IEnumerable<Condition> Flatten(Condition[] conditions) {
        System.Collections.Generic.List<Condition> result = [];
        foreach (Condition c in conditions) {
            if (c is CompoundCondition compound) {
                result.AddRange(compound.SimpleConditions);
                continue;
            }
            result.Add(c);
            
        }
        return result;


    }
    
    
    public Condition[] SimpleConditions {get;}
    
    private CompoundCondition(ConditionRTD rtd, Condition[] conditions) : base(rtd, [conditions.ToJigList()]) {
        SimpleConditions = conditions;
    }

    private CompoundCondition(ConditionRTD rtd, Record parent, Condition[] conditions) : base(rtd, parent, new SchemeValue[]{conditions.ToJigList()}.ToJigList()) {
        SimpleConditions = conditions;
    }
    
}

public class Message : Condition {
    
    private readonly String MessageString;
    
    public static Func<Condition, SchemeValue> StringAccessor = ConditionRTD.Message.Accessor(((RecordTypeDescriptor)ConditionRTD.Message).Accessor(Integer.Zero));

    public static Func<SchemeValue, Bool> Predicate = ConditionRTD.Message.Predicate();
    public Func<Condition, ISchemeValue> MessageAccessor() {
        return this.ConditionRtd.Accessor(this.RecordTypeDescriptor.Accessor(Integer.Zero));
    }

    public Message(String msg) : base(ConditionRTD.Message, [msg]) {
        MessageString = msg;
    }

}