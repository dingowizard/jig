using System.Collections.Immutable;
using System.Diagnostics.CodeAnalysis;
namespace Jig;

public delegate ILibrary LibraryFromFile(string filePath);
public delegate ILibrary LibraryFromForm(ParsedLibrary parsedLibrary);

public class LibraryLibrary {
    
    private static LibraryLibrary? _instance;

    public static void Initialize(LibraryFromFile libraryFromFile, LibraryFromForm fromFormFn, string[] libraryPaths, IEnumerable<(Symbol[], ILibrary)>? libraries = null) {
        // TODO: we only need the FromForm function?
        if (_instance == null) {
            _instance = new LibraryLibrary(libraryFromFile, fromFormFn, libraryPaths, libraries);
            return;

        }

        throw new Exception("LibraryLibrary can only be initialized once per program.");

    }

    private LibraryFromFile LibraryFromFile;
    
    public string[] LibraryPaths {get; private set;}

    public bool TryFindLibrary(ParsedImportSpec importSpec, [NotNullWhen(true)]  out ILibrary? library) {
        LibraryKey key = new LibraryKey(importSpec.Name.Select(sym => sym.Name));
        if (_dict.TryGetValue(key, out var tmp) && tmp is {} candidates) {
            var enumerable = candidates as ILibrary[] ?? candidates.ToArray();
            if (enumerable.Count() != 1) {
                throw new NotImplementedException($"Looking up {importSpec.Print()} found more than one version, but multiple versions aren't supported yet");
            }
            library = enumerable.First();
            return true;
        }
        // TODO: why was it possible to register a library twice?
        foreach (var basePath in LibraryPaths) {
            var fileStem = Path.Combine(new string[] {
                basePath
            }.Concat(importSpec.Name.Select(sym => sym.Name)).ToArray());
            var filePath = Path.ChangeExtension(fileStem, ".scm");
            // TODO: look for compiled first, make it if it doesn't exist
            if (File.Exists(filePath)) {
                library = LibraryFromFile(filePath);
                _dict.Add(new LibraryKey(importSpec.Name.Select(sym => sym.Name)), [library]);
                return true;
            }
        }
        library = null;
        return false;

    }

    public static LibraryLibrary Instance {
        get {
            if (_instance is not null) {
                return _instance;
            }
            throw new Exception();
        }
    }

    private LibraryLibrary(LibraryFromFile libraryFromFile, LibraryFromForm fromFormFn, string[] libraryPaths, IEnumerable<(Symbol[], ILibrary)>? libraries) {
        LibraryFromFile =  libraryFromFile;
        LibraryFromForm = fromFormFn;
        LibraryPaths = libraryPaths;
        if (libraries is not {} libs) return;
        foreach ((var name, ILibrary lib) in libs) {
            _dict.Add(new LibraryKey(name.Select(s => s.Name)), [lib]);
        }
    }
    public LibraryFromForm LibraryFromForm {get; set;}

    private static readonly Dictionary<LibraryKey, IEnumerable<ILibrary>> _dict = new Dictionary<LibraryKey, IEnumerable<ILibrary>>();

    public void RegisterLibrary(IEnumerable<Symbol> names, ILibrary library) {
        _dict.Add(new LibraryKey(names.Select(s => s.Name)), [library]);
    }
    public void RegisterLibrary(ParsedLibrary parsedLibrary) {
        _dict.Add(
            new LibraryKey(parsedLibrary.Name.Names.Select(id => id.Symbol.Name)),
            [LibraryFromForm(parsedLibrary)]);
    }
}

internal sealed class LibraryKey : IEquatable<LibraryKey>
{
    private readonly string[] _parts;
    private readonly int _hashCode; // cached

    // You can change StringComparer to OrdinalIgnoreCase if you want case-insensitivity
    private static readonly StringComparer PartComparer = StringComparer.Ordinal;

    public LibraryKey(IEnumerable<string> parts)
    {
        if (parts == null) throw new ArgumentNullException(nameof(parts));

        // Materialize & normalize to guard against mutation and invisible differences
        _parts = parts
            .Select(p => (p ?? string.Empty).Trim())   // trim; treat null as empty
            // .Select(p => p.Normalize())              // optionally normalize Unicode
            // .Select(p => p.ToLowerInvariant())       // optionally case-fold
            .ToArray();

        _hashCode = CalcHashCode(_parts);
    }

    public int Length => _parts.Length;

    public string this[int i] => _parts[i];

    public bool Equals(LibraryKey? other)
    {
        if (ReferenceEquals(this, other)) return true;
        if (other is null) return false;
        if (_parts.Length != other._parts.Length) return false;

        for (int i = 0; i < _parts.Length; i++)
        {
            if (!PartComparer.Equals(_parts[i], other._parts[i])) return false;
        }
        return true;
    }

    public override bool Equals(object? obj) => Equals(obj as LibraryKey);

    public override int GetHashCode() => _hashCode;

    private static int CalcHashCode(string[] parts)
    {
        // Use System.HashCode for a good mix (available in .NET Core and .NET Standard 2.1+)
        var hc = new HashCode();
        unchecked
        {
            hc.Add(parts.Length);
            foreach (var p in parts)
            {
                // Use the same comparer used in Equals
                hc.Add(p ?? string.Empty, PartComparer);
            }
        }
        return hc.ToHashCode();
    }

    public override string ToString() => string.Join(",", _parts);
}
