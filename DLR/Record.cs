using Jig;
namespace DLR;

// TODO: separate this into one common class for Jig and then another for runtime specific code (to stay here)
public class Record : Vector {

    public static Record Make(TypeDescriptor rtd, ConstructorDescriptor rcd, List args) {
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
        private static int ParameterCount(ConstructorDescriptor ParentRCD, TypeDescriptor rtd) {
            int result = rtd.Fields.Length;
            ConstructorDescriptor? parent = ParentRCD;
            while (parent is not null)
            {
                result += parent.RTD.Fields.Length;
                parent = parent.ParentRCD;
            }
            return result;
        }

    public Record(TypeDescriptor rtd, List fields) : base(fields) {
        RecordTypeDescriptor = rtd;
    }
    public Record(TypeDescriptor rtd, Record parent, List fields) : base(fields) {
        RecordTypeDescriptor = rtd;
        Parent = parent;
    }
    protected Record() {
    }

    public TypeDescriptor? RecordTypeDescriptor {get; private set;}

    protected Record? Parent {get;}

    public class TypeDescriptor : Record {

        public TypeDescriptor(List fields) : base(Base, fields) {
            if (fields.Count() != 6) {
                throw new Exception($"make-record-type-descriptor: expected six arguments, got {fields.Print()}");
            }

            if (fields.ElementAt(0) is not Symbol name) {
                throw new Exception("make-record-type-descriptor: expected first argument to be a symbol");
            }
            Name = name;

            if (fields.ElementAt(1) is Bool b) {
                if (b == Bool.True) {
                    throw new Exception($"make-record-type-descriptor: expected second argument to be #f or a record type descriptor. Got: {fields.ElementAt(1)}");
                }
            } else if (fields.ElementAt(1) is TypeDescriptor parent) {
                Parent = parent;
            } else {
                throw new Exception($"make-record-type-descriptor: expected second argument to be #f or a record type descriptor. Got: {fields.ElementAt(1)}");
            }

            if (fields.ElementAt(5) is not Vector fs) {
                throw new Exception("in TypeDescriptor cstor: expected second field to be a Vector");
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
        public new TypeDescriptor? Parent {get;} = null;

        private Thunk? IsOfMe(Delegate k, Record record) {
            if (object.ReferenceEquals(this, record.RecordTypeDescriptor)) {
                return Continuation.ApplyDelegate(k, Bool.True);
            } else {
                if (record.Parent is not null) {
                    return IsOfMe(k, record.Parent);
                }
                return Continuation.ApplyDelegate(k, Bool.False);
            }
        }

        private Thunk? GetField(Delegate k, Record record, int i) {
            if (object.ReferenceEquals(this, record.RecordTypeDescriptor)) {
                return Continuation.ApplyDelegate(k, record.Elements[i]);
            } else {
                if (record.Parent is not null) {
                    return GetField(k, record.Parent, i);
                }
                    return Builtins.Error(k, $"record access: expected record of type {this.Name}");

            }

        }



        public Procedure Predicate() {
            Builtin predicate = (k, args) => {
                if (args.Count() != 1) return Builtins.Error(k, $"{this.Name}?: expected exactly one argument but got {args.Count()}");
                ISchemeValue arg = args.ElementAt(0);
                if (arg is not Record record) {
                    return Continuation.ApplyDelegate(k, Bool.False);
                }
                return IsOfMe(k, record);
            };
            return new Procedure(predicate);

        }

        public Procedure Accessor(Integer i) {
            if (i.Value >= Fields.Length) {
                // a record with two fields has a rtd with three fields (first is name of rtd)
                // so an index of two would be point to the last field
                // TODO: the specs for the record fields should probably be in a single field
                throw new Exception("record-accessor: index out of range");

            }
            Builtin accessor = (k, args) => {
                // TODO: better error message by getting field name from spec in rtd
                if (args.Count() != 1) return Builtins.Error(k, $"record access: expected a single argument but got {args.Count()}");
                var arg = args.ElementAt(0);
                if (arg is not Record record) {
                    return Builtins.Error(k, $"record access: expected argument to be a record but got {arg}");
                }
                return GetField(k, record, i.Value);
            };
            return new Procedure(accessor);
            

        }

        public Symbol Name {get;}
        public readonly static TypeDescriptor Base = new BaseType();


        private class BaseType : TypeDescriptor  {
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

    public class ConstructorDescriptor : Record {

        public ConstructorDescriptor(List fields) : base(TypeDescriptorForConstructor, fields) {
            if (fields.ElementAt(0) is not Record.TypeDescriptor rtd) {
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

        public Procedure Constructor() {
            Builtin constructor = (k, args) => {
                if (args.Count() != ParameterCount) {
                    return Builtins.Error(k, $"{RTD.Name} constructor: expected {ParameterCount} arguments but got {args.Count()}");
                }
                return Continuation.ApplyDelegate(
                    k,
                    Record.Make(
                        RTD,
                        this,
                        args));
            };
            return new Procedure(constructor);

        }

        public TypeDescriptor RTD {get;}

        public ConstructorDescriptor? ParentRCD {get;} = null;
        public readonly static TypeDescriptor TypeDescriptorForConstructor =
            new TypeDescriptor(
                List.NewList(
                    new Symbol("rcd"),
                    Bool.False,
                    Bool.False,
                    Bool.False,
                    Bool.False,
                    new Vector()));
    }
}

