namespace VM;

public class Procedure : Jig.SchemeValue {
    
    internal Environment Environment { get; }
    
    internal Template Template { get; }
    
    public Procedure(Environment env, Template template) {
        Template = template;
        Environment = env;
        Required = template.RequiredParameterCount;
        HasRest = template.HasRestParameter;
    }
    
    public int Required { get; }
    
    public bool HasRest { get; }

    public override string Print() {
        return "#<procedure>";
    }
}