namespace Jig;

public class SyntaxPair(Syntax car, Syntax cdr) : Pair<Syntax, Syntax>(car,cdr),  IPair<Syntax,Syntax>, IPair {
    ISchemeValue IPair.Car => (SchemeValue) Car;

    ISchemeValue IPair.Cdr => (SchemeValue) Cdr;
}
