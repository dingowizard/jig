namespace Tests.Common;

public interface IInterpreter {
    string Interpret(string input);
    string InterpretUsingReadSyntax(string input);
    string InterpretSequence(string[] inputs);
    string InterpretSequenceReadSyntax(string[] inputs);
}
