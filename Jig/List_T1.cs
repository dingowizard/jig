using System.Linq;
namespace Jig;

public abstract class List<T> : List, IList<T>, IEnumerable<T> where T : Form
{
    IEnumerator<T> IEnumerable<T>.GetEnumerator() {
        IList<T> theList = this;
        while (theList is INonEmptyList<T> nonEmptyList)
        {
            yield return nonEmptyList.Car;
            theList = nonEmptyList.Cdr;
        }
    }

    public IList<T> Append(IList<T> l) => (IList<T>)((IList)this).Append(l);


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