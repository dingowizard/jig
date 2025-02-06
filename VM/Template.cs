using Jig;

namespace VM;

public class Template : Form {
    public Template(ulong[] code, Binding[] bindings, Form[] lits) {
        Slots = lits;
        Bindings = bindings;
        Code = code;
    }
    public Jig.Form[] Slots { get; }
    
    public Binding[] Bindings { get; }
    public ulong[] Code { get; }

    public override string Print() => "<#lambda template>";
}