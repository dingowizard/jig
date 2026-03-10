namespace Jig.Types;

public class TypeResolver {

    public TypeDescriptor Resolve(Type clrType) {
        if (clrType == typeof(int)) return TypeDescriptor.Int32;
        if (clrType == typeof(double)) return TypeDescriptor.Double;
        throw new NotImplementedException($"unsupported type {clrType}");
    }
    
}