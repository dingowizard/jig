namespace Jig;

public interface IRuntimeBinding {
    // TODO: mutable and immutable versions?
    public Form.Symbol Symbol {get;}
    public Form? Slot {get; set; }
}
