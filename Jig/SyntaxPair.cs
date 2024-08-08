namespace Jig;

public class SyntaxPair(Syntax car, Syntax cdr) : Pair<Syntax, Syntax>(car,cdr),  IPair<Syntax,Syntax>, IPair {
    Form IPair.Car => (Form) Car;

    Form IPair.Cdr => (Form) Cdr;
}
