using Jig.Expansion;
namespace Jig;

public class Binding {
    
    public Binding(Parameter parameter, Location location) {
        
        Parameter = parameter;
        Location = location;
    }

    public Location Location { get; }

    public Parameter Parameter { get; }
    
}