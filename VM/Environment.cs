using System.Diagnostics;
using Jig;
using Jig.Expansion;

namespace VM;

public class Environment : Form, IRuntimeEnvironment {
    
    public Dictionary<Symbol, Binding> TopLevels;

    public override string Print() => "#<environment>";
    

    private Environment(Dictionary<Form.Symbol, Binding> dict) {
        TopLevels = dict;
        Locals = null;
        Machine = new Machine(this);
    }

    private Environment(Dictionary<Symbol, Binding> topLevels, Machine runtime,  Scope? scope) {
        TopLevels = topLevels;
        Locals = scope;
        Machine = runtime;
    }
    
    public IRuntime Runtime => Machine;

    public ExpansionContext GetExpansionContext() {
        // TODO: should only have to do this once!
        return new ExpansionContext(Machine, TopLevels.Keys);
    } 

    public Machine Machine {get;}
    
    public static Environment Default { get; private set; }

    static Environment() {
        var initialBindings = new Dictionary<Symbol, Binding>();
        Default = new Environment(initialBindings);
        initialBindings[new Form.Symbol("cons")] = new Binding(new Form.Symbol("cons"), Primitives.Cons);
        initialBindings[new Form.Symbol("car")] = new Binding(new Form.Symbol("car"), Primitives.Car);
        initialBindings[new Form.Symbol("cdr")] = new Binding(new Form.Symbol("cdr"), Primitives.Cdr);
        initialBindings[new Form.Symbol("append")] = new Binding(new Form.Symbol("append"), Primitives.Append);
        initialBindings[new Form.Symbol("pair?")] = new Binding(new Form.Symbol("pair?"), Primitives.PairP);
        initialBindings[new Form.Symbol("null?")] = new Binding(new Form.Symbol("null?"), Primitives.NullP);
        initialBindings[new Form.Symbol("zero?")] = new Binding(new Form.Symbol("zero?"), Primitives.ZeroP);
        initialBindings[new Form.Symbol("call/cc")] = new Binding(new Form.Symbol("call/cc"), new Procedure(Default, VM.Builtins.CallCC));
        initialBindings[new Form.Symbol("+")] = new Binding(new Form.Symbol("+"), new Procedure(Default, VM.Builtins.Sum));
        initialBindings[new Form.Symbol("apply")] = new Binding(new Form.Symbol("apply"), new Procedure(Default, VM.Builtins.Apply));
        initialBindings[new Form.Symbol(">")] = new Binding(new Form.Symbol(">"), Primitives.GT);
        initialBindings[new Form.Symbol("<")] = new Binding(new Form.Symbol("<"), Primitives.LT);
        initialBindings[new Form.Symbol("-")] = new Binding(new Form.Symbol("-"), Primitives.Minus);
        initialBindings[new Form.Symbol("*")] = new Binding(new Form.Symbol("*"), new Procedure(Default, VM.Builtins.Product));
        initialBindings[new Form.Symbol("=")] = new Binding(new Form.Symbol("="), Primitives.NumEq);
        initialBindings[new Form.Symbol("eqv?")] = new Binding(new Form.Symbol("eqv?"), Primitives.Eqvp);
        initialBindings[new Form.Symbol("values")] = new Binding(new Form.Symbol("values"), new Procedure(Default, VM.Builtins.Values));
        initialBindings[new Form.Symbol("call-with-values")] = new Binding(new Form.Symbol("call-with-values"), new Procedure(Default, VM.Builtins.CallWithValues));
        initialBindings[new Form.Symbol("dynamic-wind")] = new Binding(new Form.Symbol("dynamic-wind"), new Procedure(Default, VM.Builtins.DynamicWind));
        initialBindings[new Form.Symbol("datum->syntax")] = new Binding(new Form.Symbol("datum->syntax"), Primitives.DatumToSyntax);
        initialBindings[new Form.Symbol("syntax->list")] = new Binding(new Form.Symbol("syntax->list"), Primitives.SyntaxToList);
    }

    

    public Environment Extend(int parameterNumber) {
        return new Environment(this.TopLevels, this.Machine,  new Scope(parameterNumber, Locals));
    }

    public void BindParameter(int i, Form pop) {
        if (Locals != null) {
            Locals.Variables[i] = pop;
        }
        else {
            throw new Exception($"tried to bind {pop.Print()} to parameter that does not exist (index = {i})");
        }
    }

    public Form GetLocal(int depth, int index) {
        return Locals.Get(depth, index);
    }

    public void DefLocal(int index, Form val) {
        if (Locals != null) {
            Locals.DefLocal(index, val);
            return;
        }

        throw new Exception("tried to add local variable, but there is no scope");

    }
    
    public void SetLocal(int depth, int index, Form val) {
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
            Variables = new Form[numVars];
        }

        public Form Get(int depth, int index) {
            Scope scope = this;
            while (depth != 0) {
                scope = scope.EnclosingScope ?? throw new Exception("ran out of scopes");
                depth--;
            }

            if (index >= scope.Variables.Length) throw new Exception("index past bounds of array");
            return scope.Variables[index];
        }

        public Scope(int numVars,  Scope? enclosingScope) {
            EnclosingScope = enclosingScope;
            Variables = new Form[numVars];
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

        public Form?[] Variables;

        public void DefLocal(int index, Form val) {
            Debug.Assert(index == Variables.Length);
            Array.Resize(ref Variables, index + 1);
            Variables[index] = val;
        }

        public void SetLocal(int depth, int index, Form val) {
            Scope scope = this;
            while (depth != 0) {
                scope = scope.EnclosingScope ?? throw new Exception("ran out of scopes");
                depth--;
            }

            if (index >= scope.Variables.Length) throw new Exception("index past bounds of array");
            scope.Variables[index] = val;
        }
    }

    Dictionary<Symbol, IRuntimeBinding> IRuntimeEnvironment.TopLevels =>
        this.TopLevels.ToDictionary(pair => pair.Key, pair => (IRuntimeBinding)pair.Value );
}

