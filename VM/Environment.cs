using System.Diagnostics;
using Jig;
using Microsoft.Scripting.Runtime;

namespace VM;

public class Environment : Form {
    
    public Dictionary<Jig.Form.Symbol, Binding> TopLevels;

    public override string Print() => "#<environment>";
    

    private Environment(Dictionary<Form.Symbol, Binding> dict) {
        TopLevels = dict;
        Locals = null;
    }

    private Environment(Dictionary<Symbol, Binding> topLevels, Scope? scope) {
        TopLevels = topLevels;
        Locals = scope;
    }

    public static Environment Default { get; } = new Environment(new Dictionary<Form.Symbol, Binding> {
        {new Form.Symbol("cons"), new Binding(new Form.Symbol("cons"), Primitives.Cons)},
        {new Form.Symbol("car"), new Binding(new Form.Symbol("car"), Primitives.Car)},
        // {new Form.Symbol("car"), new Binding(new Form.Symbol("car"), new Procedure(Default, VM.Builtins.Car))},
        {new Form.Symbol("cdr"), new Binding(new Form.Symbol("cdr"), Primitives.Cdr)},
        {new Form.Symbol("null?"), new Binding(new Form.Symbol("null?"), Primitives.NullP)},
        {new Form.Symbol("zero?"), new Binding(new Form.Symbol("zero?"), Primitives.ZeroP)},
        {new Form.Symbol("call/cc"), new Binding(new Form.Symbol("call/cc"), new Procedure(Default, VM.Builtins.CallCC))},
        {new Form.Symbol("+"), new Binding(new Form.Symbol("+"), new Procedure(Default, VM.Builtins.Sum))},
        {new Form.Symbol("values"), new Binding(new Form.Symbol("values"), new Procedure(Default, VM.Builtins.Values))},
        {new Form.Symbol("call-with-values"), new Binding(new Form.Symbol("call-with-values"), new Procedure(Default, VM.Builtins.CallWithValues))},
        {new Form.Symbol("*"), new Binding(new Form.Symbol("*"), new Procedure(Default, VM.Builtins.Product))},
    });
    

    public Environment Extend(int parameterNumber) {
        return new Environment(this.TopLevels, new Scope(parameterNumber, Locals));
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

}

