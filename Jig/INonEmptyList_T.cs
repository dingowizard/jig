namespace Jig;

public interface INonEmptyList<T> : INonEmptyList, IList<T>, IPair<T, IList<T>> where T : IForm {
    new T First {get;}
    new IList<T> Rest {get;}
}