namespace Jig;

public class Location {

    public Location(SchemeValue schemeValue) {
        Value = schemeValue;
    }

    public Location() {
        Value = null;

    }
    
    public SchemeValue? Value { get; set; }
    
}