namespace Jig;

public abstract class List<T> : List, IEnumerable<T> where T : SchemeValue
{
    IEnumerator<T> IEnumerable<T>.GetEnumerator() {
        List theList = this;
        while (!theList.IsEmpty)
        {
            yield return (T)theList.GetFirst();
            theList = theList.GetRest();
        }
    }

    public abstract override bool Equals(object? obj);

    public override int GetHashCode()
    {
        return base.GetHashCode();
    }

    public override string ToString()
    {
        return Print();
    }

}
