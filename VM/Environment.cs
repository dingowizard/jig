using Jig;

namespace VM;

public class Environment : Form {
    
    public Dictionary<Jig.Form.Symbol, Binding> TopLevels = new();

    public override string Print() => "#<environment>";
    

    private Environment(Dictionary<Form.Symbol, Binding> dict) {
        TopLevels = dict;
    }

    public static Environment Default { get; } = new Environment(new Dictionary<Form.Symbol, Binding> {
        {new Form.Symbol("a"), new Binding(new Form.Symbol("a"), Jig.Integer.One)},
        {new Form.Symbol("b"), new Binding(new Form.Symbol("b"), Jig.Integer.Two)},
        {new Form.Symbol("cons"), new Binding(new Form.Symbol("cons"), new Procedure(Default, VM.Builtins.Cons))},
        {new Form.Symbol("car"), new Binding(new Form.Symbol("car"), new Procedure(Default, VM.Builtins.Car))},
        {new Form.Symbol("cdr"), new Binding(new Form.Symbol("cdr"), new Procedure(Default, VM.Builtins.Cdr))},
        {new Form.Symbol("null?"), new Binding(new Form.Symbol("null?"), new Procedure(Default, VM.Builtins.NullP))},
        {new Form.Symbol("zero?"), new Binding(new Form.Symbol("zero?"), new Procedure(Default, VM.Builtins.ZeroP))},
        {new Form.Symbol("+"), new Binding(new Form.Symbol("+"), new Procedure(Default, VM.Builtins.Sum))},
    });

    public Environment Extend(VM.Binding[] bindings) {
        return new Environment(this, bindings);
    }

    public Environment(Environment parent, VM.Binding[] bindings) {
        TopLevels = parent.TopLevels;
    }



}

