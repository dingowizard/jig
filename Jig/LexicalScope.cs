using System.Linq.Expressions;

namespace Jig;

internal class LexicalContext {

    public LexicalContext() {
        EnclosingScope = null;
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


    public ParameterExpression? LookUp(Expr.Symbol symbol) {

        Console.WriteLine($"We're in the lexical context looking for {symbol} in {string.Join(',', Parameters.ToList())}");
        ParameterExpression? pe = Symbols.Find(tup => tup.Item1.Equals(symbol))?.Item2;
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
