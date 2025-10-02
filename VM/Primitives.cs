using Jig;

namespace VM;

public static class Primitives {
    private static void car(Machine vm) {
        SchemeValue arg = vm.Pop();
        IPair? pair = arg as IPair;
        if (pair is null) throw new Exception("car: expected argument to be a pair. Got ${arg}");
        vm.Push(vm.VAL = (SchemeValue)pair.Car);
    }
    public static Primitive Car { get; } = new("car", car, 1, false);

    private static void cdr(Machine vm) {
        SchemeValue arg = vm.Pop();
        IPair? pair = arg as IPair;
        if (pair is null) throw new Exception($"cdr: expected argument to be a pair. Got {arg}");
        vm.Push(vm.VAL = (SchemeValue)pair.Cdr);
    }

    public static Primitive Cdr { get; } = new ("cdr", cdr, 1, false);
    
    private static void cons(Machine vm) {
        SchemeValue car  = vm.Pop();
        SchemeValue cdr  = vm.Pop();
        vm.Push(vm.VAL = (SchemeValue)Pair.Cons(car, cdr));
    }

    public static Primitive Cons { get; } = new ("cons", cons, 2, false);
    
    private static void zerop(Machine vm) {
        SchemeValue schemeValue = vm.Pop();
        if (schemeValue is Number number) {
            vm.Push(vm.VAL = (number == Integer.Zero));
            return;
        }
        throw new Exception($"zero?: expected argument to be number, got {schemeValue}");
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
    public static Primitive NumEq { get; } = new("=", numEq, 1, true);

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

    public static Primitive Apply {get;} = new("apply", apply, 2, false);

    private static void apply(Machine vm) {
        var proc = vm.Pop();
        if (vm.Pop() is not Jig.List<SchemeValue> args) {
            throw new Exception("apply: expected argument to be list, got {args}");
        }

    }

    public static Primitive GT {get;} = new(">", gt, 1, true);

    // public static Primitive2 CallWValues { get; } = new(callWValues, 2, false);

    private static void gt(Machine vm) {
        
        Number arg0 = (Number)vm.Pop();
        if (vm.SP == vm.FP) {
            vm.Push(vm.VAL = Bool.True);
            return;
        }
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

    public static Primitive LT {get;} = new("<", lt, 1, true);


    public static void lt(Machine vm) {
        
        Number arg0 = (Number)vm.Pop();
        if (vm.SP == vm.FP) {
            vm.Push(vm.VAL = Bool.True);
            return;
        }
        SchemeValue result = Bool.True;
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
    public static Primitive ZeroP { get; } = new("zero?", zerop, 1, false);
    
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

    public static Primitive Expand { get; } = new("expand", expand, 1, false);

    private static void expand(Machine vm) {
        SchemeValue arg = vm.Pop();
        Syntax? stx = arg as Syntax;
        if (stx is null) {
            stx = Syntax.FromDatum(new SrcLoc(), arg);
        }

        var result = vm.Evaluator.Expander.Expand(stx, vm);
        vm.VAL = result;
        vm.Push(vm.VAL);
        return;


    }
}

public delegate void PrimitiveProcedure(Machine vm);

public class Primitive : SchemeValue
{

    public Primitive(string name, PrimitiveProcedure proc, int required, bool hasRest)
    {
        Name = name;
        Delegate = proc;
        Required = required;
        HasRest = hasRest;

    }
    
    public string Name { get; }

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
                    
                throw new Exception($"{this.Name}: wrong num args: expected {Required}, but got {vm.SP - vm.FP}. (SP = {vm.SP}; FP = {vm.FP}; stack = {vm.StackToList()})");
            }  
        }
        Delegate(vm);
    }
    public override string Print() => "#<procedure>";
    
}
