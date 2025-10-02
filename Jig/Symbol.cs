namespace Jig;

public class Symbol : SchemeValue {
    public static Symbol FromName(string name) => name switch {
        "lambda" => new Keyword.Lambda(),
        "if" => new Keyword.If(),
        "define" => new Keyword.Define(),
        "set!" => new Keyword.Set(),
        "quote" => new Keyword.Quote(), // TODO: do they have to be new? couldn't they be static instances on Keyword?
        _ => new Symbol(name),
    };

    public Symbol(string name) {
        Name = name;
    }
        
    public Expansion.Binding? Binding {get; set;} 



    public virtual string Name {get;}

    public override bool Equals(object? obj) {
        return obj switch {
            null => false,
            Symbol sym2 => this.Name == sym2.Name,
            _ => false
        };
    }

    public override int GetHashCode() {
        return Name.GetHashCode();
    }

    public override string ToString() {
        return Name;
    }

    public override string Print() => Name;
}