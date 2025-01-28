namespace VM;

public class Procedure : Jig.Form {
    
    internal Environment Environment { get; }
    
    internal Template Template { get; }
    
    public Procedure(Environment env, Template template) {
        Template = template;
        Environment = env;
    }

    public override string Print() {
        return "#<procedure>";
    }
}