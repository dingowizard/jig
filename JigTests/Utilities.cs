using Jig;
using Jig.IO;

namespace JigTests;

[TestClass]
public static class Utilities {
    // static Interpreter _interp = new Interpreter();
    static Interpreter _bareInterp = new Interpreter();
    public static IInterpreter PreludeInterp = null!;


    // public static string Interpret(string input) {
    //     return _interp.Interpret(input);
    // }

    // public static string InterpretUsingReadSyntax(string input) {
    //     return _interp.InterpretUsingReadSyntax(input);
    // }

    // public static string InterpretUsingReadSyntax(string[] inputs) {
    //     return _interp.InterpretSequenceReadSyntax(inputs);
    // }

    // public static string InterpretMultipleValues(string input) {
    //     return _interp.InterpretMultipleValues(input);
    // }

    public static string BareInterpret(string input) {
        return _bareInterp.Interpret(input);
    }

    public static string BareInterpretUsingReadSyntax(string input) {
        return _bareInterp.InterpretUsingReadSyntax(input);
    }

    public static string BareInterpretUsingReadSyntax(string[] inputs) {
        return _bareInterp.InterpretSequenceReadSyntax(inputs);
    }

    public static string BareInterpretSequenceReadSyntax(params string[] inputs) {
        return _bareInterp.InterpretSequenceReadSyntax(inputs);
    }

    public static string BareInterpretMultipleValues(string input) {
        return _bareInterp.InterpretMultipleValues(input);
    }
}

public class Interpreter : IInterpreter {
    IEnvironment Env {get;}
    public Interpreter() {
        Env = Program.TopLevel;
        SetResultAny = _setResultAny;
        SetResultOne = _setResultOne;
    }

    public string InterpretSequence(string[] inputs) {
        foreach (string input in inputs) {
            IForm? x = Jig.Reader.Reader.Read(InputPort.FromString(input));
            Assert.IsNotNull(x);
            Program.Eval(SetResultOne, x, Env);
        }
        return _result;

    }

    Continuation.ContinuationAny SetResultAny;
    Continuation.OneArgDelegate SetResultOne;

    static string _result = "";

    static Thunk? _setResultAny (params IForm[] xs) {
        IForm? first = xs[0];
        _result = first is null ? "" : first.Print();
        return null;
    }

    static Thunk? _setResultOne (IForm x) {
        _result = x.Print();
        return null;
    }

    public string InterpretSequenceReadSyntax(string[] inputs) {
        foreach (string input in inputs) {
            Form? x = Jig.Reader.Reader.ReadSyntax(InputPort.FromString(input));
            Assert.IsNotNull(x);
            Program.Eval(SetResultAny, x, Env);
        }
        return _result;
    }

    public string Interpret(string input) {
        // Continuation setResult = (x) => result = x.Print();
        IForm? x = Jig.Reader.Reader.Read(InputPort.FromString(input));
        Assert.IsNotNull(x);
        Program.Eval(SetResultAny, x, Env);
        return _result;
    }

    public string InterpretMultipleValues(string input) {
        IForm? x = Jig.Reader.Reader.Read(InputPort.FromString(input));
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
