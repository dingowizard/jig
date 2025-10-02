namespace Jig;

public interface IPair : ISchemeValue {
    ISchemeValue Car {get;}
    ISchemeValue Cdr {get;}
}
