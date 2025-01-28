namespace Jig;

public class Library { // TODO: should it inherit?
    
    private Environment env { get; }
    
    public Tuple<Form.Symbol, Form>[] Exports { get; private set; }
    
}