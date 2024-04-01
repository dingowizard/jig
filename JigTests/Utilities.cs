using Jig;
using Jig.IO;

namespace JigTests;

public static class Utilities {
    static Interpreter _interp = new Interpreter();
    public static string Interpret(string input) {
        return _interp.Interpret(input);
    }

    public static string InterpretUsingReadSyntax(string input) {
        return _interp.InterpretUsingReadSyntax(input);
    }

    public static string InterpretUsingReadSyntax(string[] inputs) {
        return _interp.InterpretSequenceReadSyntax(inputs);
    }

    public static string InterpretMultipleValues(string input) {
        return _interp.InterpretMultipleValues(input);
    }
}

public class Interpreter : IInterpreter {
    IEnvironment Env {get;}
    public Interpreter() {
        Env = Program.TopLevel;
        Program.ExecuteFile("prelude.scm", Env);
        SetResultAny = _setResultAny;
        SetResultOne = _setResultOne;
    }

    public string InterpretSequence(string[] inputs) {
        foreach (string input in inputs) {
            Expr? x = Jig.Reader.Reader.Read(InputPort.FromString(input));
            Assert.IsNotNull(x);
            Program.Eval(SetResultOne, x, Env);
        }
        return _result;

    }

    Continuation.ContinuationAny SetResultAny;
    Continuation.OneArgDelegate SetResultOne;

    static string _result = "";

    static Thunk? _setResultAny (params Expr[] xs) {
        Expr? first = xs[0];
        _result = first is null ? "" : first.Print();
        return null;
    }

    static Thunk? _setResultOne (Expr x) {
        _result = x.Print();
        return null;
    }

    public string InterpretSequenceReadSyntax(string[] inputs) {
        foreach (string input in inputs) {
            Expr? x = Jig.Reader.Reader.ReadSyntax(InputPort.FromString(input));
            Assert.IsNotNull(x);
            Program.Eval(SetResultAny, x, Env);
        }
        return _result;
    }

    public string Interpret(string input) {
        // Continuation setResult = (x) => result = x.Print();
        Expr? x = Jig.Reader.Reader.Read(InputPort.FromString(input));
        Assert.IsNotNull(x);
        Program.Eval(SetResultAny, x, Env);
        return _result;
    }

    public string InterpretMultipleValues(string input) {
        Expr? x = Jig.Reader.Reader.Read(InputPort.FromString(input));
        Assert.IsNotNull(x);
        Program.Eval(SetResultAny, x, Env);
        return _result;
    }

    public string InterpretUsingReadSyntax(string input) {
        Syntax? x = Jig.Reader.Reader.ReadSyntax(InputPort.FromString(input));
        Assert.IsNotNull(x);
        Program.Eval(SetResultOne, x, Env);
        return _result;
    }
}

public interface IInterpreter {
    string Interpret(string input);
    string InterpretUsingReadSyntax(string input);
    string InterpretSequence(string[] inputs);
    string InterpretSequenceReadSyntax(string[] inputs);
}
