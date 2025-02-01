using Jig;

namespace VM;

public class Environment : Form {
    
    public Dictionary<Jig.Form.Symbol, Binding> TopLevels = new();

    public override string Print() => "#<environment>";
    
    public Form?[] LexVar { get; }

    private Environment(Dictionary<Form.Symbol, Binding> dict) {
        TopLevels = dict;

    }

    public static Environment Default { get; } = new Environment(new Dictionary<Form.Symbol, Binding> {
        {new Form.Symbol("a"), new Binding(new Form.Symbol("a"), Jig.Integer.One)},
        {new Form.Symbol("b"), new Binding(new Form.Symbol("b"), Jig.Integer.Two)},
    });

    public Environment Extend(Jig.Binding[] bindings) {
        return new Environment(this, bindings);
    }

    public Environment(Environment parent, Jig.Binding[] bindings) {
        TopLevels = parent.TopLevels;
        LexVar = new Form?[bindings.Length];
    }



}

