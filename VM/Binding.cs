using Jig;

namespace VM;

public class Binding : IRuntimeBinding {

    public Binding(Symbol s, SchemeValue schemeValue, bool top = false) {
        Symbol = s;
        Slot = schemeValue;
        Top = top;
    }

    public Binding(Symbol s, bool top = false) {
        Symbol = s;
        Top = top;
    }

    public readonly bool Top; // false

    public Symbol Symbol {get;}
    public SchemeValue? Slot {get; set; }
}