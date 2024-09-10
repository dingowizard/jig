namespace Jig;

public interface IList<T> :  IForm, IList, IEnumerable<T> where T : IForm {
    IList<T> Append(IList<T> l);
}