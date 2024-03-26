using System.Linq.Expressions;

namespace Jig;

internal class LexicalContext {

    public LexicalContext() {
        EnclosingScope = null;
    }

    public bool AtTopLevel() {
        return EnclosingScope is null;
    }

    public LexicalContext Extend(IEnumerable<Expr.Symbol> symbols) {
        return new LexicalContext(this, symbols);
    }

    public LexicalContext Extend() {
        return new LexicalContext(this, new List<Expr.Symbol>());
    }

    private LexicalContext(LexicalContext enclosing, IEnumerable<Expr.Symbol> symbols) {
        EnclosingScope = enclosing;
        foreach (var symbol in symbols) {
            Symbols.Add(new Tuple<Expr.Symbol, ParameterExpression>(symbol, Expression.Parameter(typeof(Expr), symbol.Name)));
        }
    }

    public ParameterExpression ParameterForDefine(Expr x) {
        Expr.Symbol sym = x is Syntax.Identifier id ? id.Symbol : (Expr.Symbol)x;
        ParameterExpression? pe = Symbols.Find(tup => tup.Item1.Equals(sym))?.Item2;
        if (pe is null) {
            pe = Expression.Parameter(typeof(Expr), sym.Name);
            Symbols.Add(new Tuple<Expr.Symbol, ParameterExpression>(sym, pe));
            return pe;
        } else {
            return pe;
        }
    }


    public ParameterExpression? LookUp(Expr x) {

        Expr.Symbol symbol =
            x is Syntax stx ?
            (Expr.Symbol)Syntax.ToDatum(stx) :
            (Expr.Symbol) x;
        var candidates = Symbols.Where(tup => tup.Item1.Name==symbol.Name);
        if (symbol.Name == "l") {
            if (candidates.Count() > 0) {
                Console.WriteLine($"$LookUp: found {candidates.Count()} candidate(s) for 'l'");
                Binding? binding = candidates.ElementAt(0).Item1.Binding;
                Console.WriteLine($"$LookUp: the first candidate has binding = {(binding is null ? "null" : binding.ToString())} and the symbol has binding = {(symbol.Binding == null ? "null" : symbol.Binding.ToString())}");
            }
        }
        var candidates2 = candidates.Where(tup => tup.Item1.Binding==symbol.Binding);
        ParameterExpression? pe = null;
        if (candidates2.Count() > 0) {
            pe = candidates2.ElementAt(0)?.Item2;
        }
        if (pe is null) {
            if (EnclosingScope is null) {
                return null;
            } else {
                return EnclosingScope.LookUp(symbol);
            }

        } else {
            return pe;
        }
    }


    public ParameterExpression[] Parameters => Symbols.Select(tup => tup.Item2).ToArray();

    List<Tuple<Expr.Symbol, ParameterExpression>> Symbols {get;} = new List<Tuple<Expr.Symbol, ParameterExpression>>();

    LexicalContext? EnclosingScope {get;}


}
