using System.Diagnostics;
using Jig;
using Jig.Expansion;

namespace VM;

public class Environment : SchemeValue, IRuntimeEnvironment {
    
    public Dictionary<Parameter, Binding> TopLevels; // TODO: replace Symbol with Parameter

    public override string Print() => "#<environment>";
    

    private Environment(Dictionary<Parameter, Binding> dict) {
        TopLevels = dict;
        Locals = null;
    }

    private Environment(Dictionary<Parameter, Binding> topLevels,  Scope? scope) {
        TopLevels = topLevels;
        Locals = scope;
    }
    


    
    public static Environment Default { get; private set; }

    static Environment() {
        var initialBindings = new Dictionary<Parameter, Binding>();
        Default = new Environment(initialBindings);
        // initialBindings[new Symbol("cons")] = new Binding(new Symbol("cons"), Primitives.Cons);
        // initialBindings[new Symbol("car")] = new Binding(new Symbol("car"), Primitives.Car);
        // initialBindings[new Symbol("cdr")] = new Binding(new Symbol("cdr"), Primitives.Cdr);
        // initialBindings[new Symbol("append")] = new Binding(new Symbol("append"), Primitives.Append);
        // initialBindings[new Symbol("pair?")] = new Binding(new Symbol("pair?"), Primitives.PairP);
        // initialBindings[new Symbol("list?")] = new Binding(new Symbol("list?"), Primitives.ListP);
        // initialBindings[new Symbol("null?")] = new Binding(new Symbol("null?"), Primitives.NullP);
        // initialBindings[new Symbol("zero?")] = new Binding(new Symbol("zero?"), Primitives.ZeroP);
        // initialBindings[new Symbol("call/cc")] = new Binding(new Symbol("call/cc"), new Procedure(Default, VM.Builtins.CallCC));
        // initialBindings[new Symbol("+")] = new Binding(new Symbol("+"), new Procedure(Default, VM.Builtins.Sum));
        // initialBindings[new Symbol("apply")] = new Binding(new Symbol("apply"), new Procedure(Default, VM.Builtins.Apply));
        // initialBindings[new Symbol("expand")] = new Binding(new Symbol("expand"), Primitives.Expand);
        // initialBindings[new Symbol(">")] = new Binding(new Symbol(">"), Primitives.GT);
        // initialBindings[new Symbol("<")] = new Binding(new Symbol("<"), Primitives.LT);
        // initialBindings[new Symbol("-")] = new Binding(new Symbol("-"), Primitives.Minus);
        // initialBindings[new Symbol("*")] = new Binding(new Symbol("*"), new Procedure(Default, VM.Builtins.Product));
        // initialBindings[new Symbol("=")] = new Binding(new Symbol("="), Primitives.NumEq);
        // initialBindings[new Symbol("eqv?")] = new Binding(new Symbol("eqv?"), Primitives.Eqvp);
        // initialBindings[new Symbol("values")] = new Binding(new Symbol("values"), new Procedure(Default, VM.Builtins.Values));
        // initialBindings[new Symbol("call-with-values")] = new Binding(new Symbol("call-with-values"), new Procedure(Default, VM.Builtins.CallWithValues));
        // initialBindings[new Symbol("dynamic-wind")] = new Binding(new Symbol("dynamic-wind"), new Procedure(Default, VM.Builtins.DynamicWind));
        // initialBindings[new Symbol("datum->syntax")] = new Binding(new Symbol("datum->syntax"), Primitives.DatumToSyntax);
        // initialBindings[new Symbol("syntax->datum")] = new Binding(new Symbol("syntax->datum"), Primitives.SyntaxToDatum);
        // initialBindings[new Symbol("syntax->list")] = new Binding(new Symbol("syntax->list"), Primitives.SyntaxToList);
        // initialBindings[new Symbol("syntax-e")] = new Binding(new Symbol("syntax-e"), Primitives.SyntaxE);
        // initialBindings[new Symbol("symbol?")] = new Binding(new Symbol("symbol?"), Primitives.SymbolP);
        // initialBindings[new Symbol("symbol=?")] = new Binding(new Symbol("symbol=?"), Primitives.SymbolEqualP);
    }

    

    public Environment Extend(int parameterNumber) {
        return new Environment(this.TopLevels, new Scope(parameterNumber, Locals));
    }

    public void BindParameter(int i, SchemeValue pop) {
        if (Locals != null) {
            Locals.Variables[i] = pop;
        }
        else {
            throw new Exception($"tried to bind {pop.Print()} to parameter that does not exist (index = {i})");
        }
    }

    public SchemeValue GetLocal(int depth, int index) {
        if (Locals is not null) {
            return Locals.Get(depth, index);
        }

        throw new Exception();
    }

    public void SetLocal(int depth, int index, SchemeValue val) {
        if (Locals != null) {
            Locals.SetLocal(depth, index, val);
            return;
        }

        throw new Exception("tried to add local variable, but there is no scope");
    }
    private Scope? Locals { get; }

    private class Scope {

        public Scope(int numVars) {
            EnclosingScope = null;
            Variables = new SchemeValue[numVars];
        }

        public SchemeValue Get(int depth, int index) {
            Scope scope = this;
            while (depth != 0) {
                scope = scope.EnclosingScope ?? throw new Exception("ran out of scopes");
                depth--;
            }

            if (index >= scope.Variables.Length) throw new Exception("index past bounds of array");
            return scope.Variables[index] ?? throw new InvalidOperationException();
        }

        public Scope(int numVars,  Scope? enclosingScope) {
            EnclosingScope = enclosingScope;
            Variables = new SchemeValue[numVars];
        }
        public void AddRest(int restIndex, List rest) {
            // TODO: we should figure out when parsing a lambda expr how many params and locals it has,
            // then use this to Extend environment in apply
            // then we won't resize the array for every rest parameter and local var
            int index = Variables.Length;
            Debug.Assert(index == restIndex);
            Array.Resize(ref Variables, index + 1);
            Variables[restIndex] = rest;
        }
        
        public Scope? EnclosingScope { get; }

        public SchemeValue?[] Variables;

        public void SetLocal(int depth, int index, SchemeValue val) {
            Scope scope = this;
            while (depth != 0) {
                scope = scope.EnclosingScope ?? throw new Exception("ran out of scopes");
                depth--;
            }

            if (index >= scope.Variables.Length) throw new Exception("index past bounds of array");
            scope.Variables[index] = val;
        }
    }

    Dictionary<Parameter, Binding> IRuntimeEnvironment.TopLevels =>
        this.TopLevels.ToDictionary(pair => pair.Key, pair => pair.Value );

    public void DefineTopLevel(Parameter bindingSymbol, Binding binding)
    {
        var vmBinding = binding;
        // TODO: sigh
        Debug.Assert(vmBinding != null);
        TopLevels.Add(bindingSymbol, vmBinding);
    }

    public static Environment Minimal()
    {
        var minimalBindings = new Dictionary<Parameter, Binding>();
        return new Environment(minimalBindings);
    }

    public Location GetLocation(Parameter parameter)
    {
        throw new NotImplementedException();
    }
}

