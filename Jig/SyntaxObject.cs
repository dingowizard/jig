using System.Diagnostics.CodeAnalysis;
using System.Text;

namespace Jig;

public class Syntax : SchemeValue {
    // TODO: should this be an interface rather than a class? then e.g. Identifier : Symbol, ISyntaxObject
    //

    public static SchemeValue E(Syntax stx) {
        return stx.Expression;
    }


    public static SchemeValue ToDatum(Syntax stx) {
        if (stx is Identifier id) {
            return id.Symbol;
        }
        if (stx is Literal lit) {
            return lit.Expression;
        }
        switch (E(stx)) {
            case SyntaxPair stxPair:
                return (SchemeValue)Pair.Cons(ToDatum(stxPair.Car), ToDatum(stxPair.Cdr));
            case SyntaxList stxList:
                return stxList.Select<Syntax, SchemeValue>(ToDatum).ToJigList();
            case List { IsEmpty: true }:
                return List.Null;
            case VoidType:
                return SchemeValue.Void;
            case Symbol s: return s; // TODO: why wasn't this caught up above by stx is Identifier?
            case List.NonEmpty xs: // TODO: I think there's something wrong with how Syntax.FromDatum made this stx
                return xs.Select(x => x is Syntax sx ? ToDatum(sx) : x).ToJigList();
            // case IPair p:
            //     return ToDatum(p);
            // case List list: {
            //     IEnumerable<Syntax> slist = list.Cast<Syntax>();
            //     if (list.Count() != slist.Count()) {
            //         throw new Exception("ToDatum: expected all elements to be syntax");
            //     }
            //     return list.Cast<Syntax>().Select<Syntax, SchemeValue>(stx => ToDatum(stx)).ToJigList();
            // }
            default:
                throw new Exception($"unhandled case {E(stx)}, a {E(stx).GetType()}");
        }
    }

    // private static IPair ToDatum(IPair p) {
    //         Syntax car = p.Car as Syntax ?? throw new Exception();
    //         switch (p.Cdr) {
    //             case Syntax s:
    //                 return Pair.Cons(ToDatum(car), ToDatum(s));
    //             case SyntaxPair sp:
    //                 return Pair.Cons(ToDatum(car), ToDatum(sp));
    //             case IPair pair:
    //                 return Pair.Cons(ToDatum(car), ToDatum(pair));
    //             default:
    //                 throw new NotImplementedException($"unhandled argument type was {p.GetType()}");
    //         }
    //
    // }

    private static void AddScope(SchemeValue schemeValue, Scope scope) {
        switch (schemeValue) {
            case Syntax stx:
                AddScope(stx, scope);
                return;
            case IPair pair:
                AddScope(pair.Car, scope);
                AddScope(pair.Cdr, scope);
                return;
        }
    }

    internal static void AddScope(Syntax stx, Scope scope) {
        if (stx is Identifier id) {
            id.AddScope(scope);
            return;
        }
        if (Syntax.E(stx) is SyntaxList stxList) {
            stxList.ToList<Syntax>().ForEach(s => AddScope(s, scope));
            return;
        }
        if (Syntax.E(stx) is IPair pair) {
            AddScope(pair.Car, scope);
            AddScope(pair.Cdr, scope);
            return;
        }
        return;
    }

    private static void RemoveScope(SchemeValue schemeValue, Scope scope) {
        if (schemeValue is Syntax stx) {
            RemoveScope(stx, scope);
            return;
        } else if (schemeValue is IPair pair) {
            RemoveScope(pair.Car, scope);
            RemoveScope(pair.Cdr, scope);
            return;
        }

    }

    internal static void RemoveScope(Syntax stx, Scope scope) {
        if (stx is Identifier id) {
            id.ScopeSet.Remove(scope);
            return;
        }
        if (Syntax.E(stx) is SyntaxList stxList) {
            stxList.ToList<Syntax>().ForEach(s => RemoveScope(s, scope));
            return;
        }
        if (Syntax.E(stx) is IPair pair) {
            RemoveScope(pair.Car, scope);
            RemoveScope(pair.Cdr, scope);
            return;
        }
        return;
    }

    private static void ToggleScope(SchemeValue schemeValue, Scope scope) {
        if (schemeValue is Syntax stx) {
            ToggleScope(stx, scope);
            return;
        } else if (schemeValue is IPair pair) {
            ToggleScope(pair.Car, scope);
            ToggleScope(pair.Cdr, scope);
            return;
        }

    }
    internal static void ToggleScope(Syntax stx, Scope scope) {
        if (stx is Identifier id) {
            if (!id.ScopeSet.Remove(scope)) {
                id.ScopeSet.Add(scope);
            } 
            return;

        }
        if (Syntax.E(stx) is SyntaxList stxList) {
            stxList.ToList<Syntax>().ForEach(s => ToggleScope(s, scope));
            return;
        }
        if (Syntax.E(stx) is IPair pair) {
            ToggleScope(pair.Car, scope);
            ToggleScope(pair.Cdr, scope);
            return;

        }
    }

    public static bool ToList(Syntax stx, [NotNullWhen(returnValue: true)] out SyntaxList? stxList) {
        SchemeValue e = Syntax.E(stx);
        if (e is SyntaxList slist) {
            stxList = slist;
            return true;
        } else if (e is List l) {
            if (l is List.Empty) {
                stxList = SyntaxList.Null;
                return true;

            }
            IEnumerable<Syntax> xs = l.Select(x => new Syntax(x, null));
            stxList = xs.ToSyntaxList();
            return true;
        }
        stxList = null;
        return false;

    }

    internal void InnerStxPrint(StringBuilder sb) {
        switch (Expression) {
            case Symbol sym:
                sb.Append(sym.Name);
                break;
            case SyntaxList stxList:
                sb.Append('(');
                stxList.InnerStxPrint(sb);
                sb.Append(')');
                break;
            case SyntaxPair pair:
                sb.Append('(');
                pair.Car.InnerStxPrint(sb);
                sb.Append(" . ");
                pair.Cdr.InnerStxPrint(sb);
                sb.Append(')');
                break;
            default:
                sb.Append(Expression.Print());
                break;
        }
        
    }

    private string StxPrint()
    {
        return $"#<syntax: {Syntax.ToDatum(this).Print()}>";
        var sb = new StringBuilder("#<syntax: ");
        InnerStxPrint(sb);
        sb.Append(">");
        return sb.ToString();
    }

    public static Syntax FromDatum(SrcLoc? srcLoc, SchemeValue x) {
        switch (x) {
            case Syntax stx:
                return stx;
            case Symbol sym:
                return new Identifier(sym, srcLoc);
            case Bool:
            case Char:
            case Float:
            case Integer:
            case String:
            case Vector:
                return new Literal(x, srcLoc);
            case List list:
                return new Syntax(SyntaxList.FromIEnumerable(list.Select(x => Syntax.FromDatum(srcLoc, x))), srcLoc);
            case IPair pair:
                return new Syntax((SchemeValue)Pair.Cons(FromDatum(srcLoc, pair.Car),
                                                 FromDatum(srcLoc, pair.Cdr)),
                                  srcLoc);
            default:
                return new Syntax(x, srcLoc);
        }
    }


    public Syntax(SchemeValue expr, SrcLoc? srcLoc = null) {
        if (expr is List list) {
            
        }
        Expression = expr;
        SrcLoc = srcLoc;
    }

    public class Literal : Syntax {
        public Literal(SchemeValue x, SrcLoc? srcLoc = null) : base (x, srcLoc) {}
    }

    public override string Print() => StxPrint();

    public override string ToString() => StxPrint();

    internal virtual SchemeValue Expression {get;}

    public SrcLoc? SrcLoc {get;}
    // public LexicalContext LexicalContext {get;}

}

public struct SrcLoc(string src, int line, int column, int position, int span)
{
    public static SrcLoc WithNewEnd(SrcLoc start, int endPosition) {
        return new SrcLoc(start.Source, start.Line, start.Column, start.Position, endPosition - start.Position);
    }

    public override string ToString() => $"(srcloc {Source} {Line} {Column} {Position} {Span})";

    public readonly string Source = src;
    public readonly int Line = line;
    public readonly int Column = column;
    public readonly int Position = position;
    public readonly int Span = span;

    internal static SrcLoc Combine(SrcLoc first, SrcLoc last) {
        return new SrcLoc(first.Source,
                              first.Line,
                              first.Column,
                              first.Position,
                              (last.Position - first.Position) + last.Span);    }
}

public struct Scope {
    static int count = 0;
    int me;
    public Scope () {
        me = count ++;
    }
    public override int GetHashCode() => me;
    public override string ToString() => "scope" + me.ToString();
    public override bool Equals(object? obj) {
        if (obj is null) return false;
        if (obj is Scope sc) {
            return me == sc.me;

        } else {
            return false;
        }
    }
}
