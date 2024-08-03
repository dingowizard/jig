using Jig.IO;

namespace Jig.Reader;

public static class Reader {

    public static Form? Read(InputPort port) {
        if (port.Peek() == -1) return null;
        return Parser.ParseExpr(new TokenStream(port));
    }

    public static Syntax? ReadSyntax(InputPort port) {
        if (port.Peek() == -1) return null;
        return Parser.ParseSyntax(new TokenStream(port));
    }
}
