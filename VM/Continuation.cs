using Jig;

namespace VM;

public abstract class Continuation : SchemeValue {
    public override string Print() => "#<continuation>";

    public abstract void Pop(Machine machine);

    public abstract int Required { get; }
    
    public abstract bool HasOptional { get; }
}