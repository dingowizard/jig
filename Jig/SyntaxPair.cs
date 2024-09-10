namespace Jig;

public class SyntaxPair(Syntax car, Syntax cdr) : Pair<Syntax, Syntax>(car,cdr),  IPair<Syntax,Syntax>, IPair {
    IForm IPair.Car => (Form) Car;

    IForm IPair.Cdr => (Form) Cdr;
}
