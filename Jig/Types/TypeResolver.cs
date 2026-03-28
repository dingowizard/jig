namespace Jig.Types;

public class TypeResolver {

    public TypeDescriptor Resolve(Type clrType) {
        if (clrType == typeof(int)) return TypeDescriptor.Int32;
        if (clrType == typeof(double)) return TypeDescriptor.Double;
        if (clrType == typeof(string)) return TypeDescriptor.String;
        if (clrType == typeof(bool)) return TypeDescriptor.Boolean;
        if (clrType == typeof(char)) return TypeDescriptor.Char;
        if (clrType == typeof(SchemeValue)) return TypeDescriptor.SchemeValue;
        if (clrType == typeof(string[])) return TypeDescriptor.ArrayString;
        throw new NotImplementedException($"unsupported type {clrType}");
    }
    
}