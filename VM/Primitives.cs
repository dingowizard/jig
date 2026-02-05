using System.Runtime.InteropServices.JavaScript;
using System.Runtime.Intrinsics.X86;
using Jig;
using Jig.Expansion;
using Char = Jig.Char;
using String = Jig.String;

namespace VM;

public static class Primitives {


    public static void stackTrace(Machine vm) {


        if (!vm.ActivationStack.Empty) {
            vm.ActivationStack.StackTrace(Console.Out);
            return;
        }


        // TODO: for right now, stackTrace is being called by error, which we don't need 
        // to include as part of the stack trace.
        // So we're jumping one continuation down the call stack to start with whatever called error.
        Console.WriteLine($"stack-trace called by {vm.Template.Name}");
        Continuation cont = vm.CONT;
        while (true) {
            if (cont is TopLevelContinuation tc) {
                Console.WriteLine("called from top-level");
                break;
            }
            if (cont is SavedContinuation sc) {
                cont = sc.Saved;
                Console.WriteLine("called by saved continuation");
                continue;
            }
            if (cont is PartialContinuation pc) {
                Console.WriteLine($"called by {pc.Template.Name.Symbol.Print()} defined at {pc.Template.Name.SrcLoc?.ToString() ?? ""}");
                cont = pc.Continuation;
                continue;
            }
            throw new Exception($"unhandled case {cont.GetType()}");

        }
    }

    public static void uncheckedBinOpAdd(Machine vm) {
        Number n1 = (Number)vm.Pop();
        Number n2 = (Number)vm.Pop();
        vm.Push(vm.VAL = n1 + n2);
    }
    
    public static void uncheckedBinOpGT(Machine vm) {
        Number n1 = (Number)vm.Pop();
        Number n2 = (Number)vm.Pop();
        vm.Push(vm.VAL = n1 > n2);
    }
    
    public static void uncheckedBinOpLT(Machine vm) {
        Number n1 = (Number)vm.Pop();
        Number n2 = (Number)vm.Pop();
        vm.Push(vm.VAL = n1 < n2);
    }
    public static void uncheckedBinOpMul(Machine vm) {
        Number n1 = (Number)vm.Pop();
        Number n2 = (Number)vm.Pop();
        vm.Push(vm.VAL = n1 * n2);
    }
    
    private static void car(Machine vm) {
        IPair arg = (IPair)vm.Pop();
        vm.Push(vm.VAL = (SchemeValue)arg.Car);
    }
    public static Primitive Car { get; } = new("unchecked-car", car, 1, false);

    private static void cdr(Machine vm) {
        IPair arg = (IPair)vm.Pop();
        vm.Push(vm.VAL = (SchemeValue)arg.Cdr);
    }

    public static Primitive Cdr { get; } = new ("unchecked-cdr", cdr, 1, false);
    
    public static void cons(Machine vm) {
        SchemeValue car  = vm.Pop();
        SchemeValue cdr  = vm.Pop();
        vm.Push(vm.VAL = (SchemeValue)Pair.Cons(car, cdr));
    }

    // public static void cons2(Machine vm) {
    //     vm.Push(vm.VAL = (SchemeValue)Pair.Cons(vm.ENVT.GetArg(0), vm.ENVT.GetArg(1)));
    //     vm.CONT.Pop(vm);
    // }

    public static Primitive Cons { get; } = new ("cons", cons, 2, false);
    
    // TODO: make this unchecked
    public static void zeroP(Machine vm) {
        SchemeValue schemeValue = vm.Pop();
        if (schemeValue is Number number) {
            vm.Push(vm.VAL = (number == Integer.Zero));
            return;
        }
        throw new Exception($"zero?: expected argument to be number, got {schemeValue}");
    }

    public static void template(Machine vm) {
        SchemeValue schemeValue = vm.Pop();
        if (schemeValue is Procedure proc) {
            Array.ForEach(Disassembler.Disassemble(proc.Template), Console.WriteLine);
            return;
        }
        throw new Exception();

    }

    public static Primitive Eqvp {get;} = new("eqv?", eqvp, 2, false);

    private static void eqvp(Machine vm) {
        SchemeValue form1 = vm.Pop();
        SchemeValue form2 = vm.Pop();
        vm.Push(vm.VAL = form1.Equals(form2) ? Bool.True : Bool.False);
        return;
    }

    public static Primitive DatumToSyntax {get;} = new("datum->syntax", datumToSyntax, 2, false);

    private static void datumToSyntax(Machine vm) {

        Syntax source = vm.Pop() as Syntax ?? throw new Exception();
        ISchemeValue f = vm.Pop();
        vm.Push(vm.VAL = Syntax.FromDatum(source.SrcLoc, f));
        
    }

    private static void syntaxToDatum(Machine vm)
    {
        SchemeValue arg = vm.Pop();
        Syntax stx = arg as Syntax ?? throw new Exception($"syntax->datum: expected a syntax argument, but got {arg}, a {arg.GetType()}");
        vm.Push(vm.VAL = (SchemeValue)Syntax.ToDatum(stx));
        
    }

    public static Primitive SyntaxToDatum { get; } = new("syntax->datum", syntaxToDatum, 1, false);
        

    public static Primitive SyntaxToList {get;} = new("syntax->list", syntaxToList, 1, false);

    private static void syntaxToList(Machine vm) {

        Syntax stx = vm.Pop() as Syntax ?? throw new Exception();
        if (Syntax.E(stx) is not SyntaxList syntaxList) {
            vm.Push(vm.VAL = Bool.False);
            return;
        }
        vm.Push(vm.VAL = syntaxList);
        return;

    }
    public static Primitive SyntaxE {get;} = new("syntax-e", syntaxE, 1, false);

    private static void syntaxE(Machine vm) {

        SchemeValue arg = vm.Pop();
        Syntax stx = arg as Syntax ?? throw new Exception($"syntax-e: expected a syntax argument, but got {arg}, a {arg.GetType()}");
        vm.Push(vm.VAL = (SchemeValue)Syntax.E(stx));
        return;

    }

    public static void uncheckedBinOpDiv(Machine vm) {
        
        Number n1 = (Number)vm.Pop();
        Number n2 = (Number)vm.Pop();
        vm.Push(vm.VAL = n1 / n2);
    }
    public static void uncheckedBinOpNumEq(Machine vm) {
        
        Number n1 = (Number)vm.Pop();
        Number n2 = (Number)vm.Pop();
        vm.Push(vm.VAL = n1 == n2);
    }

    public static Primitive Append {get;} = new("append", append, 0, true);

    private static void append(Machine vm) {
        ISchemeValue result = List.Null;
        while (vm.SP > vm.FP) {
            SchemeValue arg =  (SchemeValue)vm.Pop();
            if (result is List xs) {
                result = xs.Append(arg);
            } else {
                throw new Exception($"append: expected argument to be list, got {result}");
            }

        }
        vm.VAL = (SchemeValue)result;
        vm.Push(vm.VAL);
    }

    public static Primitive PairP {get;} = new("pair?", pair_p, 1, false);

    private static void pair_p(Machine vm) {
        SchemeValue arg = vm.Pop();
        vm.VAL = arg is IPair ? Bool.True : Bool.False;
        vm.Push(vm.VAL);
        return;
    }
        
    public static Primitive ListP {get;} = new("list?", list_p, 1, false);

    private static void list_p(Machine vm) {
        SchemeValue arg = vm.Pop();
        vm.VAL = arg is IList ? Bool.True : Bool.False;
        vm.Push(vm.VAL);
        return;
    }
    public static Primitive Minus {get;} = new("-", minus, 1, true);

    private static void minus(Machine vm) {
        if (vm.SP <= vm.FP) {
            throw new Exception("-: expected at least one argument");
        }
        SchemeValue arg0 = vm.Pop();
        if (vm.SP == vm.FP) {
            vm.Push(vm.VAL = Integer.Zero - (Number)arg0 );
            return;
        }
        while (vm.SP != vm.FP) {
            Number n = (Number)vm.Pop();
            arg0 = (Number)arg0 - n;
        }
        vm.Push(vm.VAL = arg0);
        return;
    }

    public static void uncheckedBinOpMinus(Machine vm) {
        Number n1 = (Number)vm.Pop();
        Number n2 = (Number)vm.Pop();
        vm.Push(vm.VAL = n1 - n2);
    }

    public static Primitive Apply {get;} = new("apply", apply, 2, false);

    private static void apply(Machine vm) {
        // TODO: this doesn't work because nothing pushes the result onto the stack
        // You could do it by pushing down a continuation that will push the result onto the stack
        vm.VAL = vm.Pop();
        var arg1 = vm.Pop();
        if (arg1 is not Jig.List args) {
            throw new Exception($"apply: expected argument to be list, got {arg1.Print()}, a {arg1.GetType()}");
        }
        foreach (var arg in args.Reverse<ISchemeValue>()) {
            vm.Push((SchemeValue)arg);
        }
        vm.Call();
        return;

    }
    
    // public static Primitive ZeroP { get; } = new("zero?", zerop, 1, false);
    
    private static void nullp(Machine vm) {
        SchemeValue schemeValue = vm.Pop();
        if (schemeValue is List xs) {
            vm.Push(vm.VAL = xs.NullP);
            return;
        }
        vm.Push(vm.VAL = Bool.False);
        return;
    
    }

    // private static Form nullp(Form form)
    // {
    //     if (form is List xs)
    //     {
    //         return xs.NullP;
    //
    //     }
    //
    //     return Bool.False;
    // }

    // public static Primitive NullP { get; } = new(nullp);
    public static Primitive NullP { get; } = new("null?", nullp, 1, false);
    public static Primitive SymbolP { get; } = new("symbol?", symbolp, 1, false);

    private static void symbolp(Machine vm) {
        SchemeValue arg = vm.Pop();
        vm.VAL = arg is Symbol ? Bool.True : Bool.False;
        vm.Push(vm.VAL);
        return;
        
    }
    
    public static Primitive SymbolEqualP { get; } = new("symbol=?", symbolEqualP, 2, false);

    private static void symbolEqualP(Machine vm) {
        SchemeValue arg1 = vm.Pop();
        SchemeValue arg2 = vm.Pop();
        Symbol sym1 = arg1 as Symbol ?? throw new Exception("symbol=?: expected both arguments to be symbols");
        Symbol sym2 = arg2 as Symbol ?? throw new Exception("symbol=?: expected both arguments to be symbols");
        vm.VAL = sym1.Equals(sym2) ? Bool.True : Bool.False;
        vm.Push(vm.VAL);
        return;

    }
    
    public static Primitive Display {get;} = new ("display", display, 1, false);

    private static void display(Machine vm) {
        // TODO: this should optionally take a second arg, a textual output port
        // TODO: should use a Port rather than the CLR Console.Write
        var arg = vm.Pop();
        switch (arg) {
            case String str:
                Console.Write(str.Value);
                vm.Push(vm.VAL = SchemeValue.Void);
                return;
            case Char ch:
                Console.Write(ch.Value);
                vm.Push(vm.VAL = SchemeValue.Void);
                return;
            default:
                Console.Write(arg.Print());
                vm.Push(vm.VAL = SchemeValue.Void);
                return;
                
        }

    }

    public static Primitive DisplayLine {get;} = new ("displayln", displayln, 1, false);

    private static void displayln(Machine vm) {
        // TODO: this should optionally take a second arg, a textual output port
        // TODO: should use a Port rather than the CLR Console.Write
        var arg = vm.Pop();
        switch (arg) {
            case String str:
                Console.WriteLine(str.Value);
                vm.Push(vm.VAL = SchemeValue.Void);
                return;
            case Char ch:
                Console.WriteLine(ch.Value);
                vm.Push(vm.VAL = SchemeValue.Void);
                return;
            default:
                Console.WriteLine(arg.Print());
                vm.Push(vm.VAL = SchemeValue.Void);
                return;
                
        }

    }
    // TODO: is there a good reason to have all of these read-only fields?
    // why not use the constructor when instantiating them in the core library?
    public static Primitive NewLine {get;} = new ("newline", newline, 0, false);

    private static void newline(Machine vm) {
        
        // TODO: this should optionally take an arg, a textual output port
        // TODO: should use a Port rather than the CLR Console.Out
        Console.Out.WriteLine();

    }

    public static void vector(Machine vm) {
        System.Collections.Generic.List<SchemeValue> result = [];
        while (vm.SP != vm.FP) {
            result.Add(vm.Pop());
        }
        vm.Push(vm.VAL = new Vector(result.ToArray()));
    }

    public static void vectorRef(Machine vm) {
        Vector vector = vm.Pop() as Vector ?? throw new Exception("vector-ref: expected first argument to be a vector.");
        Integer index = vm.Pop() as Integer ?? throw new Exception("vector-ref: expected second argument to be an integer.");
        if (vector.TryGetAtIndex(index, out ISchemeValue? x)) {
            // TODO: gar! stop having to cast to SchemeValue (get rid of ISchemeValue or make VM fields ISchemeValue???)
            vm.Push(vm.VAL = (SchemeValue)x);
            return;
        }
        throw new Exception("vector-ref: index out of range.");
    }
    
    public static void vectorLength(Machine vm) {
        Vector vector = vm.Pop() as Vector ?? throw new Exception("vector-length: expected first argument to be a vector.");
        vm.Push(vm.VAL = vector.Length);
    }

    public static void numberP(Machine vm) {
        vm.Push(vm.VAL = vm.Pop() is Number ? Bool.True : Bool.False);
    }

    public static void charP(Machine vm) {
        vm.Push(vm.VAL = vm.Pop() is Char ? Bool.True : Bool.False);
    }
    
    public static void stringP(Machine vm) {
        vm.Push(vm.VAL = vm.Pop() is String ? Bool.True : Bool.False);
    }
    
    public static void procedureP(Machine vm) {
        SchemeValue arg = vm.Pop();
        if (arg is Procedure) {
            vm.Push(vm.VAL = Bool.True);
            return;
        }
        if (arg is Primitive) {
            // TODO: make an ICallable
            vm.Push(vm.VAL = Bool.True);
            return;
        }
        if (arg is SavedContinuation) {
            vm.Push(vm.VAL = Bool.True);
            return;
        }
        vm.Push(vm.VAL = Bool.False);
    }
    
    public static void vectorP(Machine vm) {
        // TODO: should vector? return #f when given a record?
        // The standard does not require this, but many implementations do it
        vm.Push(vm.VAL = vm.Pop() is Vector ? Bool.True : Bool.False);
    }

    public static void values(Machine vm) { }


    
    public static void make_record_type_descriptor(Machine vm) {
        // TODO: probably this is supposed to have a specific num of args (6)
        System.Collections.Generic.List<SchemeValue> args = [];
        while (vm.SP != vm.FP) {
            args.Add(vm.Pop());
        }

        vm.Push(vm.VAL = new RecordTypeDescriptor(args));

    }

    public static void record_type_descriptor_p(Machine vm) {
        System.Collections.Generic.List<SchemeValue> args = [];
        while (vm.SP != vm.FP) {
            args.Add(vm.Pop());
        }
        if (args.Count() != 1) throw new Exception( $"record-type-descriptor?: expected a single argument but got {args.Count()}");
        if (args.ElementAt(0) is RecordTypeDescriptor) {
            vm.Push(vm.VAL = Bool.True);
            return;
        }
        vm.Push(vm.VAL = Bool.False);

    }
    public static void record_constructor_descriptor_p(Machine vm) {
        System.Collections.Generic.List<SchemeValue> args = [];
        while (vm.SP != vm.FP) {
            args.Add(vm.Pop());
        }
        if (args.Count() != 1) throw new Exception( $"record-constructor-descriptor?: expected a single argument but got {args.Count()}");
        if (args.ElementAt(0) is Record.ConstructorDescriptor) {
            vm.Push(vm.VAL = Bool.True);
            return;
        }
        vm.Push(vm.VAL = Bool.False);

    }

    public static void make_record_constructor_descriptor(Machine vm) {
        
        System.Collections.Generic.List<SchemeValue> args = [];
        while (vm.SP != vm.FP) {
            args.Add(vm.Pop());
        }

        vm.Push(vm.VAL = new Record.ConstructorDescriptor(args));
    }

    public static void record_p(Machine vm) { // (Eg (record? pt) => #t
        var arg = vm.Pop();
        if (arg is Record) {
            vm.Push(vm.VAL = Bool.True);
            return;
        }
        vm.Push(vm.VAL = Bool.False);

    }

    public static void condition(Machine vm) {
        System.Collections.Generic.List<SchemeValue> args = [];
        while (vm.SP != vm.FP) {
            args.Add(vm.Pop());
        }
        vm.Push(vm.VAL = CompoundCondition.Make(args.Cast<Condition>().ToArray()));
    }

    public static void condition_p(Machine vm) {
        var arg = vm.Pop();
        if (arg is Condition) {
            vm.Push(vm.VAL = Bool.True);
            return;
        }
        vm.Push(vm.VAL = Bool.False);
    }

    public static void messageCondition_p(Machine vm) {
        var arg = vm.Pop();
        if (arg is Condition c) {
            vm.Push(vm.VAL = Message.Predicate(c));
            return;
        }
        vm.Push(vm.VAL = Bool.False);
    }
    public static void makeMessageConditionUnsafe(Machine vm) {
        String arg = (String)vm.Pop();
        vm.Push(new Message(arg));
        
    }

    public static void conditionMessage(Machine vm) {
        var arg  = (Condition)vm.Pop();
        var proc = Message.StringAccessor;
        vm.Push(vm.VAL = proc(arg));

    }
    
    public static void record_predicate(Machine vm) { // Eg (record-predicate rtd) => #<procedure>
        var arg = vm.Pop();
        if (arg is not RecordTypeDescriptor rtd) {
            throw new Exception( $"record-predicate: expected argument to be a record type descriptor but got {arg}");
        }

        vm.Push(vm.VAL = new Primitive("", Predicate, 1, false));
        return;

        void Predicate(Machine machine) {
            var predicateArg = machine.Pop();
            machine.Push(machine.VAL = rtd.Predicate()(predicateArg));
        }
    }

    public static void record_accessor(Machine vm) {
        var arg1 = vm.Pop();
        var arg2 = vm.Pop();
        if (arg1 is not RecordTypeDescriptor rtd) {
            throw new Exception($"record-predicate: expected argument to be a record type descriptor but got {arg1}");
        }
        if (arg2 is not Integer i) {
            throw new Exception($"record-predicate: expected second argument to be an integer but got {arg2}");
        }
        void Accessor(Machine machine) {
            var predicateArg = machine.Pop();
            machine.Push(machine.VAL = (SchemeValue)rtd.Accessor(i)(predicateArg));
        }

        vm.Push(vm.VAL = new Primitive("", Accessor, 1, false));

    }
    public static void record_constructor(Machine vm) {
        var arg = vm.Pop();
        if (arg is not Record.ConstructorDescriptor rcd) {
            throw new Exception($"record-constructor: expected argument to be a record constructor descriptor but got {arg}");
        }
        void Constructor(Machine machine) {
            System.Collections.Generic.List<SchemeValue> constructorArg = [];
            while (machine.SP != machine.FP) {
                constructorArg.Add(machine.Pop());
            }
            machine.Push(machine.VAL = (SchemeValue)rcd.Constructor()(constructorArg.ToJigList()));
        }

        vm.Push(vm.VAL = new Primitive("", Constructor, 1, true));

    }
    
//     public static Primitive Expand { get; } = new("expand", expand, 1, false);
//
     public static void expand(Machine vm) {
         SchemeValue arg = vm.Pop();
         Syntax? stx = arg as Syntax;
         if (stx is null) {
             stx = Syntax.FromDatum(new SrcLoc(), arg);
         }

         var result = vm.Evaluator.Expander.Expand(stx, new ExpansionContext(vm.Evaluator, vm.ENVT.TopLevels.Keys, ExpansionContextType.REPL));
         vm.VAL = result;
         vm.Push(vm.VAL);
         return;

     }

     public static void newDynEnvSlot(Machine vm) {
         SchemeValue arg = vm.Pop();
         uint slotID = vm.DynamicEnvironment.MakeDynamicVariable(arg);
         vm.Push(vm.VAL = new Integer((int)slotID)); // TODO: get rid of cast
     }

     public static void getDynEnvVal(Machine vm) {
         Integer slotID = (Integer)vm.Pop();
         vm.Push(vm.VAL = vm.DynamicEnvironment.LookUp((uint)slotID.Value)); // TODO: get rid of cast
     }

     public static void setDynEnvSlot(Machine vm) {
         Integer slotID = (Integer)vm.Pop();
         SchemeValue val = vm.Pop();
         vm.DynamicEnvironment.Set((uint)slotID.Value, val); // TODO: get rid of cast
         vm.VAL = SchemeValue.Void;
         
     }
}

public delegate void PrimitiveProcedure(Machine vm);

public class Primitive : SchemeValue, ICallable {

    public Primitive(string name, PrimitiveProcedure proc, int required, bool hasRest) {
        Name = name;
        Delegate = proc;
        Required = required;
        HasRest = hasRest;
    }
    
    public string Name { get; }

    public bool HasRest { get; }

    public int Required { get;}

    private PrimitiveProcedure Delegate { get; }

    public void Apply(Machine vm) {
        // NOTE: I think args have already been checked in logic for OpCode.Call
        // if (HasRest) {
        //     if (vm.SP - vm.FP < Required) {
        //         throw new Exception(
        //             $"wrong num args: expected at least {Required}, but got only {vm.SP - vm.FP}");
        //     }
        // } else {
        //     if (vm.SP - vm.FP != Required) {
        //             
        //         throw new Exception($"{this.Name}: wrong num args: expected {Required}, but got {vm.SP - vm.FP}. (SP = {vm.SP}; FP = {vm.FP}; stack = {vm.StackToList()})");
        //     }  
        // }
        Delegate(vm);
    }
    public override string Print() => $"#<procedure {Name}>";
    
}
