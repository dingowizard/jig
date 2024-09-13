using Jig;
using Jig.IO;

namespace PreludeTests;

[TestClass]
public static class Utilities {
    public static IInterpreter PreludeInterp = null!;

    [AssemblyInitialize]
    public static void AssemblyInitialize(TestContext tc) {
        // TODO: all tests will fail if there is an error in prelude
        // find a different way to eval this only if we are running non-core tests
        PreludeInterp = new Interpreter(withPrelude: true);

    }

}

public class Interpreter : IInterpreter {
    IEnvironment Env {get;}
    public Interpreter(bool withPrelude = true) {
        Env = Program.TopLevel;
        if (withPrelude) {
            Program.ExecuteFile("prelude.scm", Env);
        }
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
