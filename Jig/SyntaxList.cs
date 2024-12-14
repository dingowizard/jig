using System.Collections;
using System.Runtime.ExceptionServices;
using System.Security.Cryptography;
using System.Text;

namespace Jig;

public abstract class SyntaxList : List<Syntax> {


    public static new SyntaxList.Empty Null {get;} = new Empty();

    // public override Bool NullP => this is Empty ? Bool.True : Bool.False;

    public new class Empty : SyntaxList, IEmptyList<Syntax> {
        public override bool Equals(object? o) =>  o is IEmptyList;


        public override int GetHashCode()
        {
            throw new NotImplementedException();
        }
    }

    public new class NonEmpty(Syntax first, SyntaxList rest) : SyntaxList, INonEmptyList<Syntax>
    {
        public Syntax First { get; } = first;
        public SyntaxList Rest { get; } = rest;

        IList<Syntax> INonEmptyList<Syntax>.Rest => Rest;

        IList INonEmptyList.Rest => Rest;

        Syntax IPair<Syntax, IList<Syntax>>.Car => First;

        IForm IPair.Car => First;

        IList<Syntax> IPair<Syntax, IList<Syntax>>.Cdr => Rest;

        IForm IPair.Cdr => Rest;

        IForm INonEmptyList.First => First;

        public override bool Equals(object? obj) => obj switch {
            null => false,
            SyntaxList.NonEmpty list => First.Equals(list.First) && Rest.Equals(list.Rest),
            _ => false
        };

        public override int GetHashCode()
        {
            throw new NotImplementedException();
        }
    }

    public static NonEmpty Cons(Syntax car, SyntaxList cdr) {
        return new NonEmpty(car, cdr);
    }

    public static SyntaxList FromIEnumerable(IEnumerable<Syntax> stxs) {
        SyntaxList result = SyntaxList.Null;
        var arr = stxs.ToArray<Syntax>();
        for (int index = arr.Length - 1; index >= 0; index--) {
            result = new SyntaxList.NonEmpty(arr[index], result);
        }
        return result;
    }

    public static SyntaxList FromParams(params Syntax[] stxs) {
        SyntaxList result = SyntaxList.Null;
        for (int index = stxs.Length - 1; index >= 0; index--) {
            result = new SyntaxList.NonEmpty(stxs[index], result);
        }
        return result;
    }

    public void InnerStxPrint(StringBuilder sb) {
        if (this is SyntaxList.Empty) {
            return;
        }
        foreach (var stx in this.Take<Syntax>(this.Count<Syntax>() - 1)) {
            stx.InnerStxPrint(sb);
            sb.Append(' ');
        }
        this.Last<Syntax>().InnerStxPrint(sb);
    }
}

public static partial class IEnumerableExtensions {
    public static SyntaxList ToSyntaxList(this IEnumerable<Syntax> elements) {
        SyntaxList result = SyntaxList.Null;
        var arr = elements.ToArray<Syntax>();
        for (int index = arr.Length - 1; index >= 0; index--) {
            result = new SyntaxList.NonEmpty(arr[index], result);
        }
        return result;
    } 
}