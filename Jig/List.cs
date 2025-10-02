using System.Collections;

namespace Jig;

public abstract class List : SchemeValue, IEnumerable<ISchemeValue>, IList
{
    public Bool NullP => this is IEmptyList ? Bool.True : Bool.False;

    public Integer Length => this is INonEmptyList p ?  Integer.One + p.Rest.Length : Integer.Zero;

    public static Empty Null { get; } = new Empty();

    public override string Print() => $"({string.Join(" ", this.Select<ISchemeValue, string>(x => x.Print()))})";

    public static List Cons(ISchemeValue car, List cdr) {
        return new NonEmpty(car, cdr);
    }

    public static List NewList(params ISchemeValue[] args)
    {
        List result = Null;
        for (int index = args.Length - 1; index >= 0; index--)
        {
            result = new NonEmpty(args[index], result);
        }
        return result;
    }

    public static List ListFromEnumerable(IEnumerable<ISchemeValue> elements)
    {
        // WARNING: vs code says no references, but at runtime something uses method reflection to find and cache it
        List result = Null;
        var enumerable = elements as ISchemeValue[] ?? elements.ToArray();
        for (int index = enumerable.Length - 1; index >= 0; index--)
        {
            result = new NonEmpty(enumerable.ElementAt(index), result);
        }
        return result;
    }

    public override string ToString() => $"({string.Join(' ', this)})"; 




    public class Empty : List, IEmptyList {


    }

    public class NonEmpty(ISchemeValue car, List cdr) : List, INonEmptyList
    {
        public override bool Equals(object? obj)
        {
            if (obj is null) return false;
            if (obj is NonEmpty list)
            {
                return this.Car.Equals(list.Car) && this.Cdr.Equals(list.Cdr);
            }
            return false;
        }


        public ISchemeValue First { get; } = car;
        public List Rest { get; } = cdr;
        public ISchemeValue Cdr => Rest;
        public ISchemeValue Car => First;

        ISchemeValue IPair.Car => Car;

        ISchemeValue IPair.Cdr => Cdr;

        IList INonEmptyList.Rest => Rest;

        public override int GetHashCode()
        {
            return base.GetHashCode();
        }

    }


    public IEnumerator<ISchemeValue> GetEnumerator()
    {
        IList theList = this;
        while (theList is INonEmptyList nonEmptyList)
        {
            yield return nonEmptyList.Car;
            theList = nonEmptyList.Rest;
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

    public ISchemeValue Append(ISchemeValue x) {
        if (this is INonEmptyList l) {
            return x switch {
                IEmptyList => this,
                _ => Pair.Cons(l.Car, l.Rest.Append(x)),
            };
        }
        return x;
    }

    public List Append(List x) {
        if (this is NonEmpty l) {
            return x switch {
                IEmptyList => this,
                _ => List.Cons(l.Car, l.Rest.Append(x)),
            };
        }
        return x;

    }

    public IList Append(IList l)
    {
        if (this is INonEmptyList xs) {
            return l switch {
                IEmptyList => this,
                _ => (IList)Pair.Cons(xs.Car, xs.Rest.Append(l)),
            };
        }
        return l;
    }
}

public static partial class IEnumerableExtensions {
    public static List ToJigList(this IEnumerable<ISchemeValue> elements) {
        List result = List.Null;
        var enumerable = elements as ISchemeValue[] ?? elements.ToArray();
        for (int index = enumerable.Length - 1; index >= 0; index--) {
            result = new List.NonEmpty(enumerable.ElementAt(index), result);
        }
        return result;
    } 
}
