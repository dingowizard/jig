namespace Jig;

public interface INonEmptyList : IList, IPair {
    IForm First {get;}

    IList Rest {get;}
}
