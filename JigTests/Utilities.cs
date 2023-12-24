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

    public static string InterpretMultipleValues(string input) {
        return new Interpreter().InterpretMultipleValues(input);
    }
}

public class Interpreter : IInterpreter {
    IEnvironment Env {get;}
    public Interpreter() {
        Env = new Jig.Environment();
    }

    public string InterpretSequence(string[] inputs) {
        string result = "";
        Continuation.OneArgDelegate setResult = (x) => result = x.Print();
        foreach (string input in inputs) {
            Expr? x = Jig.Reader.Reader.Read(InputPort.FromString(input));
            Assert.IsNotNull(x);
            Program.Eval(setResult, x, Env);
        }
        return result;

    }

    public string InterpretSequenceReadSyntax(string[] inputs) {
        string result = "";
        Continuation.OneArgDelegate setResult = (x) => result = x.Print();
        foreach (string input in inputs) {
            Expr? x = Jig.Reader.Reader.ReadSyntax(InputPort.FromString(input));
            Assert.IsNotNull(x);
            Program.Eval(setResult, x, Env);
        }
        return result;
    }

    public string Interpret(string input) {
        string result = "";
        // Continuation setResult = (x) => result = x.Print();
        Continuation.ContinuationAny setResult = (xs) => result = xs.ElementAt(0).Print();
        Expr? x = Jig.Reader.Reader.Read(InputPort.FromString(input));
        Assert.IsNotNull(x);
        Program.Eval(setResult, x, Env);
        return result;
    }

    public string InterpretMultipleValues(string input) {
        string result = "";
        // Continuation setResult = (x) => result = x.Print();
        Continuation.ContinuationAny setResult = (xs) => result = string.Join(", ", xs.Select(x => x.Print()));
        Expr? x = Jig.Reader.Reader.Read(InputPort.FromString(input));
        Assert.IsNotNull(x);
        Program.Eval(setResult, x, Env);
        return result;
    }

    public string InterpretUsingReadSyntax(string input) {
        string result = "";
        Continuation.OneArgDelegate setResult = (x) => result = x.Print();
        Expr? x = Jig.Reader.Reader.ReadSyntax(InputPort.FromString(input));
        Assert.IsNotNull(x);
        Program.Eval(setResult, x, Env);
        return result;
    }
}

public interface IInterpreter {
    string Interpret(string input);
    string InterpretUsingReadSyntax(string input);
    string InterpretSequence(string[] inputs);
    string InterpretSequenceReadSyntax(string[] inputs);
}
