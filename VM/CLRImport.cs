using System.Linq.Expressions;
using System.Reflection;
using Jig;
using Expression = System.Linq.Expressions.Expression;
using String = Jig.String;
namespace VM;

public static class ClrImport {

    public static int Succ(int n) => n + 1;
    
    public static string Shout(string s) => s.ToUpper();
    
    public static string StringCat(string s1, string s2) => s1 + s2;

    public static void Test() {
        
        var succs = typeof(ClrImport)
            .GetMethods(BindingFlags.Public | BindingFlags.Static)
            .Where(m => m.Name == "Succ");
        System.Collections.Generic.List<ClrPrimitive> succPrimitives = [];
        foreach (var mi in succs) {
            succPrimitives.Add(new ClrPrimitive(mi));

        }
        Jig.SchemeValue result = succPrimitives.ElementAt(0).Delegate(List.ListFromEnumerable([Integer.One]));
        Console.WriteLine($"We compiled a ClrPrimitive for Succ and passed it 1. The result is {result.Print()}");
        var shout = typeof(ClrImport).GetMethod(nameof(Shout), BindingFlags.Static | BindingFlags.Public);

        result = new ClrPrimitive(shout).Delegate(List.ListFromEnumerable([new Jig.String("i don't want to shout")]));
        Console.WriteLine($"called shout with 'i don't want to shout': {result}");
        var stringCat  = typeof(ClrImport).GetMethod(nameof(StringCat), BindingFlags.Static | BindingFlags.Public);
        result = new ClrPrimitive(stringCat).Delegate(List.ListFromEnumerable([new String("beep"), new String("boop")]));
        Console.WriteLine($"called stringcat with 'beep' and 'boop': {result}");
    }
    
}

public delegate Jig.SchemeValue ClrPrimitiveUnsafe(Jig.List args);

public class ClrPrimitive : ICallable {
    public ClrPrimitive(MethodInfo mi) {
        var parameters = mi.GetParameters();                                                                                                        
        int count = parameters.Length;                          
        HasRest = parameters.Length > 0 &&                                                                                                           
                         parameters[^1].IsDefined(typeof(ParamArrayAttribute), false);
        Required = HasRest ? count - 1 : count;
        Delegate = MakeDelegate(mi);
    }
    private ClrPrimitiveUnsafe MakeDelegate(MethodInfo mi) {
        
        
        var listParam = Expression.Parameter(typeof(Jig.List), "args");                                                                                     
        var arrayVar = Expression.Variable(typeof(Jig.SchemeValue[]), "argsArray");                                                                            
        
        
        return Expression.Lambda<ClrPrimitiveUnsafe>(
            Expression.Block(
                [arrayVar],
                ArgListToArray(arrayVar, listParam),
                MakeCallExpression(mi, arrayVar)),
            listParam).Compile();
                                                          
    }

    private static BinaryExpression ArgListToArray(ParameterExpression arrayParam, ParameterExpression listParam) {
        return Expression.Assign(
            arrayParam,
            Expression.Call(
                typeof(Enumerable)
                    .GetMethod("ToArray")!
                    .MakeGenericMethod(typeof(Jig.SchemeValue)),
                listParam));
    }

    private static Expression MakeCallExpression(MethodInfo mi, ParameterExpression arrayArgsParam) {
        
        return WrapReturn( mi,Expression.Call(mi, MakeArgs(mi, arrayArgsParam)));
    }

    private static Expression[] MakeArgs(MethodInfo mi, ParameterExpression args) {
        var parameters = mi.GetParameters();
        System.Collections.Generic.List<Expression> argsList = [];
        int i = 0;
        foreach (var p in parameters) {
            argsList.Add(Expression.Property(
                Expression.Convert(
                    Expression.ArrayIndex(args, Expression.Constant(i)),
                    MappedType(p.ParameterType)),
                "Value"));
            i++;

        }
        return argsList.ToArray();
    }

    private static Expression WrapReturn(MethodInfo mi, Expression expr) {
        
        // TODO: probably it shouldn't be the constructor but a static method on every scheme type
        // that know how to get the SchemeValue from the corresponding clr type
        return Expression.New(MappedType(mi.ReturnType).GetConstructor([mi.ReturnType])!, expr);
        
    }

    private static Type MappedType(Type t) {
          if (t == typeof(int)) return typeof(Jig.Integer);                                                                                               
          if (t == typeof(string)) return typeof(Jig.String);                                                                                           
          throw new Exception($"unhandled type: {t}");
      }

    public ClrPrimitiveUnsafe Delegate {get;}
    
    
    public int Required {get;}
    public bool HasRest {get;}
}