using Jig;

namespace VM;

public class Template : Form {
    public Template(ulong[] code, Binding[] globals, Form[] lits) {
        Slots = lits;
        Globals = globals;
        Code = code;
    }
    public Jig.Form[] Slots { get; }
    
    public Binding[] Globals { get; }
    public ulong[] Code { get; }

    public override string Print() => "<#lambda template>";
}