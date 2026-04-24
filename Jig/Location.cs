using Jig.Expansion;
namespace Jig;

public class Location {

    public class Later : Location {
        public Later(Parameter.Maybe parameter, IRuntimeEnvironment env) : base() {
            // TODO: could arguments and locals also be like this, so that we don't need
            // a separate ARG instruction?
            _location = new Lazy<Location>(() => {
                // TODO: I think a runtime env should have a method for looking up a parameter!
                if (env.TopLevels.TryGetValue(parameter, out Binding? b)) {
                    return b.Location;
                }
                throw new Exception($"couldn't find {parameter.Print()} in top-levels");
            });
        }

        private Lazy<Location> _location;

        public override SchemeValue Value => _location.Value.Value; 
        

    }

    public Location(SchemeValue schemeValue) {
        Value = schemeValue;
    }

    public Location() {
        // TODO: why does this exist? variables defined in lambda bodies?
        Value = null;

    }
    
    public virtual SchemeValue? Value { get; set; }
    
}