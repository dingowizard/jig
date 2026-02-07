using System.Collections;
using System.Runtime.ExceptionServices;
using System.Security.Cryptography;
using System.Text;

namespace Jig;

public abstract class SyntaxList : List<Syntax> {


    public static new SyntaxList.Empty Null {get;} = new Empty();

    // public override Bool NullP => this is Empty ? Bool.True : Bool.False;

    public new class Empty : SyntaxList {
        public override bool IsEmpty => true;
        public override bool Equals(object? o) =>  o is List { IsEmpty: true };


        public override int GetHashCode()
        {
            throw new NotImplementedException();
        }
    }

    public new class NonEmpty(Syntax first, SyntaxList rest) : SyntaxList, IPair
    {
        public override bool IsEmpty => false;

        public new Syntax First { get; } = first;
        public new SyntaxList Rest { get; } = rest;

        // Base class virtual overrides (for polymorphic access via List)
        internal override SchemeValue GetFirst() => First;
        internal override List GetRest() => Rest;

        SchemeValue IPair.Car => First;

        SchemeValue IPair.Cdr => Rest;

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

    public override IPair Prepend(SchemeValue car) {
        if (car is Syntax stx)
            return SyntaxList.Cons(stx, this);
        return new List.NonEmpty(car, this);
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
