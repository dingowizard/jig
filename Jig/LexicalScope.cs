using System.Linq.Expressions;

namespace Jig;
using System.Collections.Generic;

internal class LexicalContext {

    public LexicalContext() {
        EnclosingScope = null;
    }

    public bool AtTopLevel() {
        return EnclosingScope is null;
    }

    public LexicalContext Extend(IEnumerable<Form.Symbol> symbols) {
        return new LexicalContext(this, symbols);
    }

    public LexicalContext Extend() {
        return new LexicalContext(this, new System.Collections.Generic.List<Form.Symbol>());
    }

    private LexicalContext(LexicalContext enclosing, IEnumerable<Form.Symbol> symbols) {
        EnclosingScope = enclosing;
        foreach (var symbol in symbols) {
            Symbols.Add(new Tuple<Form.Symbol, ParameterExpression>(symbol, Expression.Parameter(typeof(IForm), symbol.Name)));
        }
    }

    public ParameterExpression ParameterForDefine(IForm x) {
        Form.Symbol sym = x is Syntax.Identifier id ? id.Symbol : (Form.Symbol)x;
        ParameterExpression? pe = Symbols.Find(tup => tup.Item1.Equals(sym))?.Item2;
        if (pe is null) {
            pe = Expression.Parameter(typeof(IForm), sym.Name);
            Symbols.Add(new Tuple<Form.Symbol, ParameterExpression>(sym, pe));
            return pe;
        } else {
            return pe;
        }
    }


    public ParameterExpression? LookUp(IForm x) {

        Form.Symbol symbol =
            x is Syntax stx ?
            (Form.Symbol)Syntax.E(stx) :
            (Form.Symbol) x;
        var candidates = Symbols.Where(tup => tup.Item1.Name==symbol.Name);
        var enumerable = candidates as Tuple<Form.Symbol, ParameterExpression>[] ?? candidates.ToArray();
        /*
        if (symbol.Name == "y") {
                Console.WriteLine($"\tLookUp: found {enumerable.Length} candidate(s) for 'y' ({symbol.Binding}");
                if (enumerable.Length != 0) {
                    Binding? binding = enumerable[0].Item1.Binding;
                    if (binding is not null) {
                        Console.WriteLine($"\tLookUp: the first candidate has binding = {binding.ToString()} and the symbol has binding = {(symbol.Binding == null ? "null" : symbol.Binding.ToString())}");
                        Console.WriteLine(
                            $"\t\tThe first candidate's binding and the lookup symbol's are ==: {symbol.Binding == binding}");
                        Console.WriteLine($"\t\tThe first candidate's binding and the lookup symbol's are Equals: {symbol.Binding?.Equals(binding)}");
                        Console.WriteLine($"\t\tThe first candidate's binding and the lookup symbol's are ReferenceEquals: {ReferenceEquals(symbol.Binding,binding)}");
                    }

                }
        }
        */
        var candidates2 = enumerable.Where(tup => tup.Item1.Binding==symbol.Binding);
        ParameterExpression? pe = null;
        if (candidates2.Any()) {
            pe = candidates2.ElementAt(0)?.Item2;
        }
        if (pe is null) {
            if (EnclosingScope is null) {
                // if (symbol.Name == "y") {
                //     Console.WriteLine($"\tLookUp: couldn't find 'y'");
                // }
                return null;
            } else {
                return EnclosingScope.LookUp(symbol);
            }

        } else {
            // if (symbol.Name == "y") {
            //     Console.WriteLine($"\tLookUp: about to return {pe}");
            // }
            return pe;
        }
    }


    public ParameterExpression[] Parameters => Symbols.Select(tup => tup.Item2).ToArray();

    System.Collections.Generic.List<Tuple<Form.Symbol, ParameterExpression>> Symbols {get;} = [];

    LexicalContext? EnclosingScope {get;}


}
