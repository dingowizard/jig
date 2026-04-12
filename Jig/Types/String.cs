using Jig.Types;
namespace Jig;

public class String(string s) : LiteralExpr<string>(s) {
    public override string Print() {
        // TODO: handle special chars like \n
        return "\"" + Value + "\"";
    }

    public static TypeDescriptor TypeDescriptor = new SchemeValueTypeDescriptor<String>();
}

