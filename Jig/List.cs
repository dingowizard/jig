using System.Collections;
using System.Reflection.Metadata.Ecma335;

namespace Jig;

public abstract class List : Form, IEnumerable<IForm>, IList
{
    public Bool NullP => this is IEmptyList ? Bool.True : Bool.False;

    public Integer Length => this is INonEmptyList p ?  Integer.One + p.Rest.Length : Integer.Zero;

    public static Empty Null { get; } = new Empty();

    public override string Print() => $"({string.Join(" ", this.Select<IForm, string>(x => x.Print()))})";

    public static List Cons(IForm car, List cdr) {
        return new List.NonEmpty(car, cdr);
    }

    public static List NewList(params IForm[] args)
    {
        List result = Null;
        for (int index = args.Length - 1; index >= 0; index--)
        {
            result = new NonEmpty(args[index], result);
        }
        return result;
    }

    public static List ListFromEnumerable(IEnumerable<IForm> elements)
    {
        // WARNING: vs code says no references, but at runtime something uses method reflection to find and cache it
        List result = Null;
        for (int index = elements.Count() - 1; index >= 0; index--)
        {
            result = new NonEmpty(elements.ElementAt(index), result);
        }
        return result;
    }

    public override string ToString() => $"({string.Join(' ', this)})"; 




    public class Empty : List, IEmptyList {


    }

    public class NonEmpty(IForm car, List cdr) : List, INonEmptyList, IPair
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


        public IForm First { get; } = car;
        public List Rest { get; } = cdr;
        public IForm Cdr => Rest;
        public IForm Car => First;

        IForm IPair.Car => Car;

        IForm IPair.Cdr => Cdr;

        IList INonEmptyList.Rest => Rest;

        public override int GetHashCode()
        {
            return base.GetHashCode();
        }

    }


    public IEnumerator<IForm> GetEnumerator()
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

    public IForm Append(IForm x) {
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
    public static Jig.List ToJigList(this IEnumerable<IForm> elements) {
        List result = List.Null;
        for (int index = elements.Count() - 1; index >= 0; index--) {
            result = new List.NonEmpty(elements.ElementAt(index), result);
        }
        return result;
    } 
}
