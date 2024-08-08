using System.Linq;
namespace Jig;

public abstract class List<T> : List, IList<T>, IEnumerable<T> where T : Form
{
    IEnumerator<T> IEnumerable<T>.GetEnumerator() {
        List theList = this;
        while (theList is NonEmpty<T> nonEmptyList)
        {
            yield return nonEmptyList.Car;
            theList = nonEmptyList.Rest;
        }
    }

    public class NonEmpty<T> : List<T>, IPair<T, List<T>> where T : Form
    {
        public NonEmpty(T car, List<T> cdr)
        {
            Car = car;
            Cdr = cdr;
            Rest = cdr;
        }

        public override bool Equals(object? obj)
        {
            if (obj is null) return false;
            if (obj is NonEmpty list)
            {
                return this.Car.Equals(list.Car) && this.Cdr.Equals(list.Cdr);
            }
            return false;
        }

        public T Car { get; set; }
        public List<T> Cdr { get; set; }

        public List Rest { get; }
        public override int GetHashCode()
        {
            return base.GetHashCode();
        }

        public override string ToString()
        {
            return Print();
        }

        public override string Print()
        {
            return "(" + string.Join(" ", ((IEnumerable<Form>)this).Select(el => el.Print())) + ")";
        }

    }
}