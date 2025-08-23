namespace Jig;

public class Library { // TODO: should it inherit?
    
    private Environment env { get; }
    
    public Tuple<Symbol, Form>[] Exports { get; private set; }
    
}