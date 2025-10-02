namespace Jig;

public interface INonEmptyList : IList, IPair {
    ISchemeValue First {get;}

    IList Rest {get;}
}
