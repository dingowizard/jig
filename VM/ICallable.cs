namespace VM;

public interface ICallable {
    // TODO: should this be in Jig rather than VM?
    int Required {get;}
    bool HasRest {get;}
    
}