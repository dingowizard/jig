using Jig;

namespace VM;

public static class Primitives {
    private static Form car(Form form) {
        if (form is IPair p) {
            return (Form)p.Car;
        }
        throw new Exception("car: expected argument to be pair, got {form}");
    }
    public static Primitive Car { get; } = new Primitive(car);

    private static Form cdr(Form form) {
        if (form is IPair p) {
            return (Form)p.Cdr;
        }
        throw new Exception("cdr: expected argument to be pair, got {form}");
    }

    public static Primitive Cdr { get; } = new Primitive(cdr);
    
    private static Form cons(Form car, Form cdr) {
        return (Form)Pair.Cons(car, cdr);
    }

    public static Primitive Cons { get; } = new Primitive(cons);
    
    private static Form zerop(Form form)
    {
        if (form is Number number) {
            return number == Integer.Zero;
        }
        throw new Exception("zero?: expected argument to be number, got {form}");
    }

    // private static void callWValues(Machine vm)
    // {
    //     var producer = (Procedure)vm.Pop();
    //     var continuationProc = (Procedure)vm.Pop();
    //     vm.CONT = new PartialContinuationForCallWithValues(
    //         continuationProc.Template,
    //         0,
    //         continuationProc.Environment,
    //         vm.FP,
    //         vm.CONT,
    //         continuationProc.Required,
    //         continuationProc.HasRest);
    //     vm.VAL = producer;
    //     vm.Call();
    //     // this doesn't work because the procedure is expected to push its results, but it doesn't have any yet
    //     
    // }
    public static Primitive2 Eqvp {get;} = new(eqvp, 2, false);

    private static void eqvp(Machine vm) {
        Form form1 = vm.Pop();
        Form form2 = vm.Pop();
        vm.Push(vm.VAL = form1.Equals(form2) ? Bool.True : Bool.False);
        return;
    }
        

    public static Primitive2 NumEq { get; } = new(numEq, 1, true);

    private static void numEq(Machine vm)
    {
        // TODO: what if args aren't numbers?
        Number number1 = (Number)vm.Pop();
        while (vm.SP > vm.FP)
        {
            Number number2 = (Number)vm.Pop();
            if ((number1 != number2).Value)
            {
                // you don't have to keep testing arguments for equality
                // so throw away rest of args to call
                vm.SP = vm.FP;
                vm.Push(Bool.False);
                return;

            }
            
        }

        vm.Push(Bool.True);
        return;

    }

    public static Primitive2 Append {get;} = new(append, 0, true);

    private static void append(Machine vm) {
        IForm result = List.Null;
        while (vm.SP > vm.FP) {
            Form arg =  (Form)vm.Pop();
            if (result is List xs) {
                result = xs.Append(arg);
            } else {
                throw new Exception($"append: expected argument to be list, got {result}");
            }

        }
        vm.VAL = (Form)result;
        vm.Push(vm.VAL);
    }

    public static Primitive2 PairP {get;} = new(pair_p, 1, false);

    private static void pair_p(Machine vm) {
        Form arg = vm.Pop();
        vm.VAL = arg is IPair ? Bool.True : Bool.False;
        vm.Push(vm.VAL);
        return;
    }
        
    public static Primitive2 Minus {get;} = new(minus, 1, true);

    private static void minus(Machine vm) {
        if (vm.SP <= vm.FP) {
            throw new Exception("-: expected at least one argument");
        }
        Form arg0 = vm.Pop();
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

    public static Primitive2 Apply {get;} = new(apply, 2, false);

    private static void apply(Machine vm) {
        var proc = vm.Pop();
        if (vm.Pop() is not Jig.List<Form> args) {
            throw new Exception("apply: expected argument to be list, got {args}");
        }

    }

    public static Primitive2 GT {get;} = new(gt, 1, true);

    // public static Primitive2 CallWValues { get; } = new(callWValues, 2, false);

    private static void gt(Machine vm) {
        
        // TODO: I think the argument cound was already checked, no?
        if (vm.SP <= vm.FP) {
            throw new Exception("<: expected at least one argument");
        }
        Number arg0 = (Number)vm.Pop();
        if (vm.SP == vm.FP) {
            vm.Push(vm.VAL = Bool.True);
            return;
        }
        Form result = Bool.True;
        while (vm.SP != vm.FP) {
            Number n = (Number)vm.Pop();
            if ((arg0 <= n).Value) {
                // throw away rest of arguments
                vm.SP = vm.FP;
                vm.Push(vm.VAL = Bool.False);
                return;
            }
            arg0 = n;
        }
        vm.Push(vm.VAL = Bool.True);
        return;
    }

    public static Primitive2 LT {get;} = new(lt, 1, true);

    // public static Primitive2 CallWValues { get; } = new(callWValues, 2, false);

    public static void lt(Machine vm) {
        
        if (vm.SP <= vm.FP) {
            throw new Exception("<: expected at least one argument");
        }
        Number arg0 = (Number)vm.Pop();
        if (vm.SP == vm.FP) {
            vm.Push(vm.VAL = Bool.True);
            return;
        }
        Form result = Bool.True;
        while (vm.SP != vm.FP) {
            Number n = (Number)vm.Pop();
            if ((arg0 >= n).Value) {
                // throw away rest of arguments
                vm.SP = vm.FP;
                vm.Push(vm.VAL = Bool.False);
                return;
            }
            arg0 = n;
        }
        vm.Push(vm.VAL = Bool.True);
        return;
    }
    public static Primitive ZeroP { get; } = new(zerop);
    
    private static void nullp(Machine vm)
    {
        Form form = vm.Pop();
        if (form is List xs) {
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
    public static Primitive2 NullP { get; } = new(nullp, 1, false);
}

public delegate void PrimitiveProcedure(Machine vm);

public class Primitive2 : Form
{

    public Primitive2(PrimitiveProcedure proc, int required, bool hasRest)
    {
        Delegate = proc;
        Required = required;
        HasRest = hasRest;

    }

    public bool HasRest { get; }

    public int Required { get;}

    private PrimitiveProcedure Delegate { get; }

    public void Apply(Machine vm)
    {
        if (HasRest) {
            if (vm.SP - vm.FP < Required) {
                throw new Exception(
                    $"wrong num args: expected at least {Required}, but got only {vm.SP - vm.FP}");
            }
        } else {
            if (vm.SP - vm.FP != Required) {
                    
                throw new Exception($"wrong num args: expected {Required}, but got {vm.SP - vm.FP}. (SP = {vm.SP}; FP = {vm.FP}; stack = {vm.StackToList()})");
            }  
        }
        Delegate(vm);
    }
    public override string Print() => "#<procedure>";
    
}

public class Primitive : Form {
    public Primitive(Delegate fn) {
        Delegate = fn;
    }

    public Delegate Delegate { get;}
    public override string Print() => "#<procedure>";

}