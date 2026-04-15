namespace Jig;

public interface IInterOp {

    ILibrary ImportClrNameSpace(string name);

    ILibrary LibraryFromType(Type type);
}