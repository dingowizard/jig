namespace Jig.Primitives;

public class TestProcedures {
    // TODO: I think this class is just helpful at the moment for testing the type resolver at the REPL
    // prob should be deleted later and no actual tests or scheme library code should use it

    public static String SymbolToString(Symbol sym) {
        return new String(sym.Name);
    }
    
    public static Symbol StringToSymbol(String str) {
        return new Symbol(str.Value);
    }

    public static Char StringToChar(String str) {
        return new Char(str.Value[0]);
    }

    public static String CharToString(Char c) {
        return new String(c.ToString());
    }
}