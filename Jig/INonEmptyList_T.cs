namespace Jig;

public interface INonEmptyList<T> : INonEmptyList, IList<T>, IPair<T, IList<T>> where T : ISchemeValue {
    new T First {get;}
    new IList<T> Rest {get;}
}