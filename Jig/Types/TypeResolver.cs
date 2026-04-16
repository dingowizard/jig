using System.Diagnostics;
using System.Linq.Expressions;
namespace Jig.Types;

public class TypeResolver {

    public TypeDescriptor Resolve(Type clrType) {
        if (clrType == typeof(int)) return TypeDescriptor.Int32;
        if (clrType == typeof(double)) return TypeDescriptor.Double;
        if (clrType == typeof(string)) return TypeDescriptor.String;
        if (clrType == typeof(bool)) return TypeDescriptor.Boolean;
        if (clrType == typeof(char)) return TypeDescriptor.Char;
        if (clrType == typeof(SchemeValue)) return TypeDescriptor.SchemeValue; // TODO: make this a static property on SchemeValue
        if (clrType == typeof(String)) return String.TypeDescriptor;
        if (clrType == typeof(Symbol)) return Symbol.TypeDescriptor;
        if (clrType == typeof(Char)) return Char.TypeDescriptor;
        if (clrType == typeof(Bool)) return Bool.TypeDescriptor;
        if (clrType == typeof(Integer)) return Integer.TypeDescriptor;
        if (clrType == typeof(Vector)) return  Vector.TypeDescriptor;
        // if (clrType == typeof(string[])) return TypeDescriptor.ArrayString;
        
        if (_typeCache.TryGetValue(clrType, out var tDesc)) return tDesc;

        if (clrType.IsArray) {
            var elementType = clrType.GetElementType();
            var td = TypeDescriptorForArray(elementType); 
            _typeCache.Add(clrType, td);
            return td;
            
        }

        if (clrType.IsConstructedGenericType) {
            // this will include generic interfaces like IList<int>
            var genTypeDef = clrType.GetGenericTypeDefinition();
            throw new NotImplementedException($"clrType = {clrType.Name}");

        }
        if (clrType.IsInterface) {
            throw new NotImplementedException();
        }
        // here we should have some non-array, non-generic, non-interface that wasn't hard coded at the top
        
        // the argument to the constructor 
        // public TypeDescriptor(Func<T, TU> func) where T is clrType and TU is LiteralExpr<T> (obj) => new LiteralExpr<T>(obj) 
        var param = System.Linq.Expressions.Expression.Parameter(clrType, "obj");

        // var cast = System.Linq.Expressions.Expression.Convert(param, clrType);

        var litExprT = typeof(LiteralExpr<>).MakeGenericType(clrType);
        var ctor = litExprT.GetConstructor(new[] { clrType });
        Debug.Assert(ctor != null);

        var body = System.Linq.Expressions.Expression.New(ctor, param);
        
        Type funcType = typeof(Func<,>).MakeGenericType(clrType, litExprT);

        var lambda = System.Linq.Expressions.Expression.Lambda(funcType, body, param);

        var compiled = lambda.Compile();
        // now we have this: (o) => new LiteralExpr<ClrType>((CrType) o). We can use this in LiteralDescriptor<T, TU>() once we build it
        var closedType = typeof(TypeDescriptor<,>)
            .MakeGenericType(clrType, litExprT);

        TypeDescriptor literalTypeDescriptor = Activator.CreateInstance( closedType, new object[] {compiled}) as TypeDescriptor ?? throw new Exception();
        _typeCache.Add(clrType, literalTypeDescriptor);
        return literalTypeDescriptor;
    }
    private TypeDescriptor TypeDescriptorForArray(Type? elementType) {
        return new ArrayTypeDescriptor(Resolve(elementType));
    }

    private Dictionary<Type, TypeDescriptor> _typeCache = new Dictionary<Type, TypeDescriptor>();
    
}