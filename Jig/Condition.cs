using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
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

    public override string Print() {
        Debug.Assert(RecordTypeDescriptor is not null);
        string name = RecordTypeDescriptor.Name.Name;
        System.Collections.Generic.List<string> fields = [];
        int n = 0;
        foreach (var field in RecordTypeDescriptor.Fields) {

            fields.Add(field.Item1.Name + ": " + Elements[n].Print());
            n++;


        }
        return $"#<{name} {string.Join(", ", fields)}>";

    }

    public bool TryGet<T>([NotNullWhen(true)] out T? condition) where T : Condition {
        if (this is CompoundCondition cc) {
            foreach (var c in cc.SimpleConditions) {
                if (c is T result) {
                    condition = result;
                    return true;
                }
            }
            condition = null;
            return false;
        }
        if (this is T t) {
            condition = t;
            return true;
        }
        condition = null;
        return false;
        
    }
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

    public override string Print() {
        string name;
        string[] fields;
        if (SimpleConditions.Any(x => x is AssertionViolation)) {
            var ass = Array.Find(SimpleConditions, x => x is AssertionViolation);
            name = "&assertion";
            fields = SimpleConditions.ToList().Where(x => x != ass).Select(x => x.Print()).ToArray();
        } else {
            name = "&condition";
            fields = SimpleConditions.ToList().Select(x => x.Print()).ToArray();
        }
        return $"#<{name} {string.Join(" ", fields)}>";
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

public class AssertionViolation : Condition {

    public AssertionViolation() : base(ConditionRTD.Assertion, []) {}

}