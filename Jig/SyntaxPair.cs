namespace Jig;

public class SyntaxPair(Syntax car, Syntax cdr) : Pair(car, cdr) {
    public new Syntax Car { get; } = car;
    public new Syntax Cdr { get; } = cdr;
}
