using Jig;
namespace VM;

public class Procedure : Jig.SchemeValue {
    
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
            result.Add(env.LookUpLocation(parameter));
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