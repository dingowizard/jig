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
        if (_dict.TryGetValue(importSpec.Name, out var tmp) && tmp is {} candidates) {
            var enumerable = candidates as ILibrary[] ?? candidates.ToArray();
            if (enumerable.Count() != 1) {
                throw new NotImplementedException($"Looking up {importSpec.Print()} found more than one version, but multiple versions aren't supported yet");
            }
            library = enumerable.First();
            return true;
        }
        foreach (var basePath in LibraryPaths) {
            var fileStem = Path.Combine(new string[] {
                basePath
            }.Concat(importSpec.Name.Select(sym => sym.Name)).ToArray());
            var filePath = Path.ChangeExtension(fileStem, ".scm");
            // TODO: look for compiled first, make it if it doesn't exist
            if (File.Exists(filePath)) {
                library = LibraryFromFile(filePath);
                _dict.Add(importSpec.Name, [library]);
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
            _dict.Add(name, [lib]);
        }
    }
    public LibraryFromForm LibraryFromForm {get; set;}

    private static readonly Dictionary<Symbol[], IEnumerable<ILibrary>> _dict = new Dictionary<Symbol[], IEnumerable<ILibrary>>();

    public void RegisterLibrary(ParsedLibrary parsedLibrary) {
        _dict.Add(
            parsedLibrary.Name.Names.Select(id => id.Symbol).ToArray(),
            [LibraryFromForm(parsedLibrary)]);
    }
}