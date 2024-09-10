using System.Collections;

namespace Jig;

public interface IList : IForm, IEnumerable {
    Bool NullP {get;}
    Integer Length {get;}

    IForm Append (IForm x);
    IList Append (IList l);

}
