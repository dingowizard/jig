namespace VM;

public class Binding : Jig.Form {
    public Jig.Form.Symbol Symbol { get; }
    public Jig.Form Slot { get; set; }

    public override string Print() {
        return "<binding>";
    }
}