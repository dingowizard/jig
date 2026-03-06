using Jig.Expansion;
namespace Jig;

// TODO: this seems like a runtime concept
public class Binding {
    
    public Binding(Parameter parameter, Location location) {
        
        Parameter = parameter;
        Location = location;
    }

    public Location Location { get; }

    public Parameter Parameter { get; }
    
}