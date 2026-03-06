using System.Diagnostics;

namespace Jig.IO;

public class InputPort : IDisposable {

    public static InputPort FromString(string str) {
        return new InputPort(new StringReader(str));
    }

    public InputPort() {
        _reader = Console.In;
        Source = _reader.ToString() ?? "string";
        Line = 1;
        Column = 0;
        Position = 1;
    }

    public InputPort(TextReader reader) {
        _reader = reader;
        Source = _reader.ToString() ?? "string";
        Line = 1;
        Column = 0;
        Position = 1;
    }

    public InputPort(string path) {
        _reader = File.OpenText(path);
        Source = path ?? "string";
        Line = 1;
        Column = 0;
        Position = 1;

    }

    public int Peek() {
        return _reader.Peek();
    }

    public int Read() {
        int read = _reader.Read();
        Trace.WriteLine($"Reader.Read: {read.ToString()}");
        char result = (char) read;
        if (result == '\n') {
            Line ++;
            Column = 0;
            Position ++;
        } else {
            Column ++;
            Position ++;
        }
        return read;
    }

    public void Dispose() {
        _reader.Dispose();
    }

    TextReader _reader {get;}

    public string Source {get; private set;}

    public int Line {get; private set;}

    public int Column {get; private set;}

    public int Position {get; private set;}
}
