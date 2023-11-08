using Jig.IO;

namespace Jig.Reader;

public class Reader {

    public static Expr Read(InputPort port) {
        return Parser.ParseExpr(new TokenStream(port));
    }

}
