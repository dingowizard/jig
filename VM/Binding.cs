using Jig;

namespace VM;

public class Binding {

    public Binding(Form.Symbol s, Form form) {
        Symbol = s;
        Slot = form;
    }

    public Binding(Jig.Binding binding) {
        Symbol = binding.Symbol;
    }

    public Binding(Form.Symbol s) {
        Symbol = s;
    }

    public Jig.Form.Symbol Symbol;
    public Jig.Form? Slot;

}