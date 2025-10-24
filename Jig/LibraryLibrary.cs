namespace Jig;

public class LibraryLibrary
{
    private static LibraryLibrary? _instance;

    public static void Initialize() {
        if (_instance == null) {
            _instance = new LibraryLibrary();
            return;

        }

        throw new Exception();

    }

    public ILibrary LookUp(ParsedLibraryName libraryName) {
        throw new NotImplementedException();
    }

    public static LibraryLibrary Instance
    {
        get
        {
            if (_instance is not null) {
                return _instance;
            } else {
                throw new Exception();
            }
        }
    }

    private LibraryLibrary()
    {
    }

}