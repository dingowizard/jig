using Jig;
using Jig.IO;

namespace JigTests;

public static class Utilities {
    public static string Interpret(string input) {
        return new Interpreter().Interpret(input);
    }

    public static string InterpretUsingReadSyntax(string input) {
        return new Interpreter().InterpretUsingReadSyntax(input);
    }
}

public class Interpreter : IInterpreter {
    IEnvironment Env {get;}
    public Interpreter() {
        Env = new Jig.Environment();
    }

    public string Interpret(string input) {
        string result = "";
        Continuation setResult = (x) => result = x.Print();
        Expr? x = Jig.Reader.Reader.Read(InputPort.FromString(input));
        Assert.IsNotNull(x);
        Program.Eval(setResult, x, Env);
        return result;
    }

    public string InterpretUsingReadSyntax(string input) {
        string result = "";
        Continuation setResult = (x) => result = x.Print();
        Expr? x = Jig.Reader.Reader.ReadSyntax(InputPort.FromString(input));
        Assert.IsNotNull(x);
        Program.Eval(setResult, x, Env);
        return result;
    }
}

public interface IInterpreter {
    string Interpret(string input);
    string InterpretUsingReadSyntax(string input);
}
