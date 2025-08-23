using Jig;

namespace VM;

public class Binding : IRuntimeBinding {

    public Binding(Form.Symbol s, Form form, bool top = false) {
        Symbol = s;
        Slot = form;
        Top = top;
    }

    public Binding(Jig.Binding binding, bool top = false) {
        Symbol = binding.Symbol;
        Top = top;
    }

    public Binding(Form.Symbol s, bool top = false) {
        Symbol = s;
        Top = top;
    }

    public readonly bool Top; // false

    public Form.Symbol Symbol {get;}
    public Form? Slot {get; set; }
}