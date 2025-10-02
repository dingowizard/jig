namespace Jig;

public interface IList<T> :  ISchemeValue, IList, IEnumerable<T> where T : ISchemeValue {
    IList<T> Append(IList<T> l);
}