using Jig;

namespace VM;

public class Binding : Jig.Form {

    public Binding(Form.Symbol s, Form form) {
        Symbol = s;
        Slot = form;
    }
    public Jig.Form.Symbol Symbol { get; }
    public Jig.Form Slot { get; set; }

    public override string Print() {
        return "<binding>";
    }
}