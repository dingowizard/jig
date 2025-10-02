namespace Jig;

public interface IRuntimeBinding {
    // TODO: mutable and immutable versions?
    public Symbol Symbol {get;}
    public SchemeValue? Slot {get; set; }
}
