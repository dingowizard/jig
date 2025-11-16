namespace Jig;

public class RecordTypeDescriptor : Record {

    public RecordTypeDescriptor(IEnumerable<ISchemeValue> fields) : base(Base, fields) {
        // TODO: it's bad that we're doing argument checking here.
        // all that logic should be in the runtime procedures.
        // this shouldn't detect errors or throw exceptions
        // same comment for records
        if (fields.Count() != 6) {
            throw new Exception($"make-record-type-descriptor: expected six arguments, got {fields.ToJigList().Print()}");
        }

        if (fields.ElementAt(0) is not Symbol name) {
            throw new Exception("make-record-type-descriptor: expected first argument to be a symbol");
        }
        Name = name;

        if (fields.ElementAt(1) is Bool b) {
            if (b == Bool.True) {
                throw new Exception($"make-record-type-descriptor: expected second argument to be #f or a record type descriptor. Got: {fields.ElementAt(1)}");
            }
        } else if (fields.ElementAt(1) is RecordTypeDescriptor parent) {
            Parent = parent;
        } else {
            throw new Exception($"make-record-type-descriptor: expected second argument to be #f or a record type descriptor. Got: {fields.ElementAt(1)}");
        }
        

        var xs = fields.ElementAt(5);
        if (fields.ElementAt(5) is not Vector fs) {
            throw new Exception($"make-record-type-descriptor: expected sixth argument to be vector. Got {xs.Print()}, a {xs.GetType()}");

        }
        System.Collections.Generic.List<Tuple<Symbol, bool>> listFields = [];
        foreach (var f in fs) {
            if (f is not List.NonEmpty listField) {
                throw new Exception();
            }
            if (listField.Count() != 2) {
                throw new Exception("field spec should have two members");
            }
            if (listField.ElementAt(0) is not Symbol mutability) {
                throw new Exception("expected a symbol value for field mutability");
            }
            if (listField.ElementAt(1) is not Symbol fieldName) {
                throw new Exception("expected a symbol value for field mutability");
            }
            bool mut = mutability.Equals(new Symbol("mutable")) || (mutability.Equals(new Symbol("immutable")) ? false : throw new Exception());
            listFields.Add(new Tuple<Symbol, bool>(fieldName, mut));
        }
        Fields = [.. listFields];
    }
    public Tuple<Symbol, bool>[] Fields {get;}
    public new RecordTypeDescriptor? Parent {get;} = null;

    private Bool IsOfMe(Record record) {
        if (object.ReferenceEquals(this, record.RecordTypeDescriptor)) {
            return Bool.True;
        } else {
            if (record.Parent is not null) {
                return IsOfMe(record.Parent);
            }
            return Bool.False;
        }
    }

    private ISchemeValue GetField(Record record, int i) {
        if (ReferenceEquals(this, record.RecordTypeDescriptor)) {
            return record.Elements[i];
        }
        if (record.Parent is not null) {
            return GetField(record.Parent, i);
        }
        throw new Exception($"record access: expected record of type {this.Name}");
    }



    public Func<SchemeValue, Bool> Predicate() {
        return (arg) => {
            if (arg is not Record record) {
                return Bool.False;
            }
            return IsOfMe(record);
        };

    }

    public Func<SchemeValue, ISchemeValue> Accessor(Integer i) {
        if (i.Value >= Fields.Length) {
            // a record with two fields has a rtd with three fields (first is name of rtd)
            // so an index of two would be point to the last field
            // TODO: the specs for the record fields should probably be in a single field
            throw new Exception("record-accessor: index out of range");

        }
        return (arg) => {
            // TODO: better error message by getting field name from spec in rtd
            if (arg is not Record record) {
                throw new Exception($"record access: expected argument to be a record but got {arg}");
            }
            return GetField(record, i.Value);
        };

    }

    public Symbol Name {get;}
    public readonly static RecordTypeDescriptor Base = new BaseType();


    private class BaseType : RecordTypeDescriptor  {
        public BaseType() : base(List.NewList(
            new Symbol("base-rtd"),
            Bool.False,
            Bool.False,
            Bool.False,
            Bool.False,
            new Vector()))
        {
            RecordTypeDescriptor = this;
        }

        public override string Print() => "#!base-rtd";

    }

    public override string Print() => $"#<record type {Name}>";
}