using Jig.IO;
using System.Diagnostics;

namespace Jig.Reader;

public static class Reader {

    public static ISchemeValue? Read(InputPort port) {
        // TODO: all of these should return an EOF object rather than null
        if (port.Peek() == -1) return null;
        return Parser.ParseExpr(new TokenStream(port));
    }

    public static Syntax? ReadSyntax(InputPort port) {
        Trace.WriteLine("ReadSyntax called");
        Trace.Flush();
        try {
            if (port.Peek() == -1) return null;
        }
        catch (Exception x) {
            Trace.WriteLine("ReadSyntax: exception while peeking:");
            Trace.WriteLine(x.Message);
            Trace.Flush();
            return null;

        }
        Trace.WriteLine("ReadSyntax: peek was not -1");
        Trace.Flush();
        return Parser.ParseSyntax(new TokenStream(port));
    }
    
    public static IEnumerable<Syntax> ReadFileSyntax(InputPort port) {
        while (true) {
            var stx = ReadSyntax(port);
            if (stx == null) break;
            yield return stx;
        }
    }
}
