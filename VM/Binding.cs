using Jig;

namespace VM;

public class Binding : IRuntimeBinding {

    public Binding(Symbol s, SchemeValue schemeValue) {
        Symbol = s;
        Location = new Location(schemeValue);
    }

    public Symbol Symbol {get;}
    public Location Location {get; set; }
}