namespace Jig;

// TODO: separate this into one common class for Jig and then another for runtime specific code (to stay here)
public class Record : Vector {

    public static Record Make(RecordTypeDescriptor rtd, ConstructorDescriptor rcd, List args) {
        // Maybe this should be on ConstructorDescriptor. ParameterCount logic is repeated there
        if (rcd.ParentRCD is null) {
            return new Record(rtd, args);
        }
        int ownArgNum = rtd.Fields.Length;
        int parentArgNum = ParameterCount(rcd.ParentRCD, rtd) - ownArgNum;
        return new Record(
                rtd,
                Make(rcd.ParentRCD.RTD, rcd.ParentRCD, args.Take(parentArgNum).ToJigList()),
                args.Skip(parentArgNum).ToJigList());
            
    }
        private static int ParameterCount(ConstructorDescriptor ParentRCD, RecordTypeDescriptor rtd) {
            int result = rtd.Fields.Length;
            ConstructorDescriptor? parent = ParentRCD;
            while (parent is not null)
            {
                result += parent.RTD.Fields.Length;
                parent = parent.ParentRCD;
            }
            return result;
        }

    public Record(RecordTypeDescriptor rtd, IEnumerable<ISchemeValue> fields) : base(fields) {
        RecordTypeDescriptor = rtd;
    }
    public Record(RecordTypeDescriptor rtd, Record parent, List fields) : base(fields) {
        RecordTypeDescriptor = rtd;
        Parent = parent;
    }
    protected Record() {
    }

    public RecordTypeDescriptor? RecordTypeDescriptor {get; protected set;}

    protected internal Record? Parent {get;}

    public class ConstructorDescriptor : Record {

        public ConstructorDescriptor(IEnumerable<ISchemeValue> fields) : base(TypeDescriptorForConstructor, fields) {
            if (fields.ElementAt(0) is not RecordTypeDescriptor rtd) {
                throw new Exception("in ConstructorDescriptor cstor: expected first field to be a record type descriptor");
            }

            RTD = rtd; // note: the ConstructorDescriptor is a record that has two rtds: its own and
                        // a field that contains the RTD for the record it is the constructor of (RTD)
            if (fields.ElementAt(1) is Bool b) {
                if (b == Bool.True) {
                    throw new Exception($"make-record-constructor-descriptor: expected second argument to be #f or a record constructor descriptor. Got: {fields.ElementAt(1)}");
                }
            } else if (fields.ElementAt(1) is ConstructorDescriptor parent) {
                ParentRCD = parent;
            } else {
                throw new Exception($"make-record-constructor-descriptor: expected second argument to be #f or a record constructor descriptor. Got: {fields.ElementAt(1)}");
            }

        }

        private new int ParameterCount {
            get
            {
                int result = RTD.Fields.Length;
                ConstructorDescriptor? parent = ParentRCD;
                while (parent is not null)
                {
                    result += parent.RTD.Fields.Length;
                    parent = parent.ParentRCD;
                }
                return result;
            }
        }

        public Func<List, Record> Constructor() {
            return (args) => {
                if (args.Count() != ParameterCount) { // TODO: someone else has to check the argument count
                    throw new Exception($"{RTD.Name} constructor: expected {ParameterCount} arguments but got {args.Count()}");
                }
                return Record.Make(
                        RTD,
                        this,
                        args);
            };

        }

        public RecordTypeDescriptor RTD {get;}

        public ConstructorDescriptor? ParentRCD {get;} = null;
        
        public readonly static RecordTypeDescriptor TypeDescriptorForConstructor =
            new RecordTypeDescriptor(
                List.NewList(
                    new Symbol("rcd"),
                    Bool.False,
                    Bool.False,
                    Bool.False,
                    Bool.False,
                    new Vector()));
    }
}