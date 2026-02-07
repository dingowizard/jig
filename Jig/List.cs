using System.Collections;

namespace Jig;

public abstract class List : SchemeValue, IEnumerable<SchemeValue>
{
    public abstract bool IsEmpty { get; }

    internal virtual SchemeValue GetFirst() => throw new InvalidOperationException("Empty list");
    internal virtual List GetRest() => throw new InvalidOperationException("Empty list");

    public virtual SchemeValue First => GetFirst();
    public virtual List Rest => GetRest();

    public Bool NullP => IsEmpty ? Bool.True : Bool.False;

    public Integer Length => !IsEmpty ?  Integer.One + GetRest().Length : Integer.Zero;

    public static Empty Null { get; } = new Empty();

    public override string Print() => $"({string.Join(" ", this.Select<SchemeValue, string>(x => x.Print()))})";

    public static List Cons(SchemeValue car, List cdr) {
        return new NonEmpty(car, cdr);
    }

    public static List NewList(params SchemeValue[] args)
    {
        List result = Null;
        for (int index = args.Length - 1; index >= 0; index--)
        {
            result = new NonEmpty(args[index], result);
        }
        return result;
    }

    public static List ListFromEnumerable(IEnumerable<SchemeValue> elements)
    {
        // WARNING: vs code says no references, but at runtime something uses method reflection to find and cache it
        List result = Null;
        var enumerable = elements as SchemeValue[] ?? elements.ToArray();
        for (int index = enumerable.Length - 1; index >= 0; index--)
        {
            result = new NonEmpty(enumerable.ElementAt(index), result);
        }
        return result;
    }

    public override string ToString() => $"({string.Join(' ', this)})";




    public class Empty : List {
        public override bool IsEmpty => true;
    }

    public class NonEmpty(SchemeValue car, List cdr) : List, IPair
    {
        public override bool IsEmpty => false;

        public override bool Equals(object? obj)
        {
            if (obj is null) return false;
            if (obj is NonEmpty list)
            {
                return this.Car.Equals(list.Car) && this.Cdr.Equals(list.Cdr);
            }
            return false;
        }


        public override SchemeValue First { get; } = car;
        public override List Rest { get; } = cdr;

        internal override SchemeValue GetFirst() => First;
        internal override List GetRest() => Rest;

        public SchemeValue Cdr => Rest;
        public SchemeValue Car => First;

        public override int GetHashCode()
        {
            return base.GetHashCode();
        }

    }


    public IEnumerator<SchemeValue> GetEnumerator()
    {
        List theList = this;
        while (!theList.IsEmpty)
        {
            yield return theList.GetFirst();
            theList = theList.GetRest();
        }

    }

    IEnumerator IEnumerable.GetEnumerator()
    {
        return this.GetEnumerator();
    }

    public override int GetHashCode()
    {
        int hash = 19;
        unchecked
        {
            foreach (var expr in this)
            {
                hash = hash * 31 + expr.GetHashCode();

            }
        }
        return hash;
    }

    public SchemeValue Append(SchemeValue x) {
        if (this is NonEmpty l) {
            return x switch {
                List { IsEmpty: true } => this,
                _ => (SchemeValue)Pair.Cons(l.Car, l.Rest.Append(x)),
            };
        }
        return x;
    }

    public List Append(List x) {
        if (this is NonEmpty l) {
            return x switch {
                List { IsEmpty: true } => this,
                _ => List.Cons(l.Car, l.Rest.Append(x)),
            };
        }
        return x;

    }
}

public static partial class IEnumerableExtensions {
    public static List ToJigList(this IEnumerable<SchemeValue> elements) {
        List result = List.Null;
        var enumerable = elements as SchemeValue[] ?? elements.ToArray();
        for (int index = enumerable.Length - 1; index >= 0; index--) {
            result = new List.NonEmpty(enumerable.ElementAt(index), result);
        }
        return result;
    }
}
