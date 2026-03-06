using Jig;
using Jig.Expansion;
namespace VM;

public class Procedure : SchemeValue, ICallable {
    
    internal Template Template { get; }
    
    public Procedure(Environment env, Template template) {
        Template = template;
        Locations = SaveVarLocations(env, template);
        Required = template.RequiredParameterCount;
        HasRest = template.HasRestParameter;
        Environment = env;
    }

    private Location[] SaveVarLocations(Environment env, Template template) {
        var result = new System.Collections.Generic.List<Location>();
        foreach (var parameter in template.Vars) {
            Location loc;
            if (parameter is Parameter.Maybe maybe) {
                loc = new Location.Later(maybe, env);
                result.Add(loc);
            } else {
                try {
                    loc = env.LookUpLocation(parameter);
                    result.Add(loc);
                }
                catch {
                    Console.WriteLine($"Error while looking up variable {parameter.Symbol}");
                    throw;
                }
            }
        }
        return result.ToArray();
            
    }
    
    public Environment Environment { get; }


    public Location[] Locations { get; }

    public int Required { get; }
    
    public bool HasRest { get; }

    public override string Print() {
        return "#<procedure>";
    }
}