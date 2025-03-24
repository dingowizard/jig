using Jig;

namespace VM;

public class Binding {

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

    public Jig.Form.Symbol Symbol;
    public Jig.Form? Slot;
    public bool Top = false;

}