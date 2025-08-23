using Jig;

namespace VM;

public class Binding : IRuntimeBinding {

    public Binding(Symbol s, Form form, bool top = false) {
        Symbol = s;
        Slot = form;
        Top = top;
    }

    public Binding(Symbol s, bool top = false) {
        Symbol = s;
        Top = top;
    }

    public readonly bool Top; // false

    public Symbol Symbol {get;}
    public Form? Slot {get; set; }
}