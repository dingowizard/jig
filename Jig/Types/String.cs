namespace Jig;

public class String(string s) : LiteralExpr<string>(s) {
    public override string Print() {
        // TODO: handle special chars like \n
        return "\"" + Value + "\"";
    }
}

