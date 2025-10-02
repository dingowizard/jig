using System.Collections;

namespace Jig;

public interface IList : ISchemeValue, IEnumerable {
    Bool NullP {get;}
    Integer Length {get;}

    ISchemeValue Append (ISchemeValue x);
    IList Append (IList l);

}
