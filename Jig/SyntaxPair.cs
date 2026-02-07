namespace Jig;

public class SyntaxPair(Syntax car, Syntax cdr) : Pair<Syntax, Syntax>(car,cdr),  IPair<Syntax,Syntax>, IPair {
    SchemeValue IPair.Car => (SchemeValue) Car;

    SchemeValue IPair.Cdr => (SchemeValue) Cdr;
}
