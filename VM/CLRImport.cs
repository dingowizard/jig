using System.Linq.Expressions;
using System.Reflection;
using Jig;
using Expression = System.Linq.Expressions.Expression;
using String = Jig.String;
namespace VM;

public static class ClrImport {
    
    
    static IEnumerable<Type> ResolveClrName(string name) {
        // try as a type first
        var t = Type.GetType(name);
        if (t != null)
            return new[] { t };

        // otherwise treat as namespace
        return AppDomain.CurrentDomain
            .GetAssemblies()
            .SelectMany(a => a.GetTypes())
            .Where(t => t.Namespace == name
                        || t.Namespace?.StartsWith(name + ".") == true);
    }

    public static Library ImportClrNameSpace(string name) {
        var ts = ResolveClrName(name);
        // for now, we're going to select only those static methods that receive double or int arguments and return double results
        var methodInfos =
            ts.SelectMany(t => t.GetMethods(BindingFlags.Public | BindingFlags.Static))
                .Where(mi => (mi.ReturnType == typeof(double) || mi.ReturnType == typeof(int)) &&
                             mi.GetParameters().All(p => p.ParameterType == typeof(double) || p.ParameterType == typeof(int)));
        // get const double fields (PI and E)
        var constantFields =
            ts.SelectMany(t => t.GetFields(BindingFlags.Public | BindingFlags.Static))
                .Where(fi => fi is {IsLiteral: true, IsInitOnly: false} && fi.FieldType == typeof(double));
        System.Collections.Generic.List<Binding> bindings = [];
        int index = 0;
        foreach (var f in constantFields) {
            var v = f.GetRawConstantValue();
            SchemeValue schemeValue = ConvertToSchemeValue(v);

            string fullName = /* f.DeclaringType.FullName +  "." + */  f.Name;
            var bg = new Binding(new Jig.Expansion.Parameter(new Symbol(fullName), [], 0, index++, null),
                new Location(schemeValue));
            bindings.Add(bg);
        }
        foreach (var mi in methodInfos) {
            var clrPrimitive = new ClrPrimitive(mi);
            System.Collections.Generic.List<string> paramNameAndTypes = [];
            // TODO: stringbuilder for name
            foreach (var p in mi.GetParameters()) {
                paramNameAndTypes.Add(p.ParameterType.Name);
            }
            string ps = string.Join("->", paramNameAndTypes);
            string returnType = mi.ReturnType.Name;
            string fullName = /* mi.DeclaringType.FullName +  "."  + */ mi.Name + "/" + ps + (ps != "" ? "->" + returnType : returnType);
            var bg = new Binding(new Jig.Expansion.Parameter(new Symbol(fullName), [], 0, index++, null),
                new Location(clrPrimitive));
            bindings.Add(bg);


        }
        
        return new Library(bindings, []);

    }
    private static SchemeValue ConvertToSchemeValue(object? o) {
        switch  (o) {
            case null: return SchemeValue.Void; // TODO: this is probably wrong. :(
            case double d: return new Jig.Float(d);
            default:
                throw new NotImplementedException("unhandled type: " + o.GetType());
        }
    }

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

        result = new ClrPrimitive(shout!).Delegate(List.ListFromEnumerable([new Jig.String("i don't want to shout")]));
        Console.WriteLine($"called shout with 'i don't want to shout': {result}");
        var stringCat  = typeof(ClrImport).GetMethod(nameof(StringCat), BindingFlags.Static | BindingFlags.Public);
        result = new ClrPrimitive(stringCat!).Delegate(List.ListFromEnumerable([new String("beep"), new String("boop")]));
        Console.WriteLine($"called stringcat with 'beep' and 'boop': {result}");
    }
    
}

public delegate Jig.SchemeValue ClrPrimitiveUnsafe(Jig.List args);

public class ClrPrimitive : SchemeValue, ICallable {
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
                Expression.Assign(arrayVar, ArgListToArray(arrayVar, listParam)),
                MakeCallExpression(mi, arrayVar)),
            listParam).Compile();
                                                          
    }

    private static Expression ArgListToArray(ParameterExpression arrayParam, ParameterExpression listParam) {
            return Expression.Call(
                typeof(Enumerable)
                    .GetMethod("ToArray")!
                    .MakeGenericMethod(typeof(Jig.SchemeValue)),
                listParam);
    }

    private static Expression MakeCallExpression(MethodInfo mi, ParameterExpression arrayArgsParam) {
        
        return WrapReturn( mi,Expression.Call(mi, MakeArgs(mi, arrayArgsParam)));
    }

    private static Expression[] MakeArgs(MethodInfo mi, ParameterExpression args) {
        var parameters = mi.GetParameters();
        System.Collections.Generic.List<Expression> argsList = [];
        int i = 0;
        foreach (var p in parameters) {
            argsList.Add(WrapParameter(args, i, p));
            i++;

        }
        return argsList.ToArray();
    }

    private static Expression WrapParameter(ParameterExpression argsArrayParam, int argIndex, ParameterInfo p) {
        // TODO: for the moment, this is an unnecessary level of indirection, but may need more complicated logic
        // for things that don't have a Value property.

        // NOTE: if the parameter type is a double, we also should be able to take ints

        if (p.ParameterType == typeof(double)) {
            var isInt = Expression.TypeIs(Expression.ArrayIndex(argsArrayParam, Expression.Constant(argIndex)), typeof(Jig.Integer));
            var asInt = Expression.Convert(Expression.ArrayIndex(argsArrayParam, Expression.Constant(argIndex)), typeof(Jig.Integer));
            var convertMethod = typeof(Jig.Integer).GetMethod("op_Implicit", new[]
            {
                typeof(Jig.Integer),
            });
            var convertToDouble = Expression.Call(convertMethod, asInt);
            var conditionalExpr = Expression.Condition(isInt, convertToDouble, Expression.Convert(Expression.ArrayIndex(argsArrayParam, Expression.Constant(argIndex)), typeof(Jig.Float)));
            return Expression.Property(
                Expression.Convert(
                    conditionalExpr,
                    MappedType(p.ParameterType)),
                "Value");
        }
        return Expression.Property(
            Expression.Convert(
                Expression.ArrayIndex(argsArrayParam, Expression.Constant(argIndex)),
                MappedType(p.ParameterType)),
            "Value");


    }

    private static Expression WrapReturn(MethodInfo mi, Expression expr) {
        
        // TODO: probably it shouldn't be the constructor but a static method on every scheme type
        // that know how to get the SchemeValue from the corresponding clr type
        return Expression.New(MappedType(mi.ReturnType).GetConstructor([mi.ReturnType])!, expr);
        
    }

    private static Type MappedType(Type t) {
          if (t == typeof(int)) return typeof(Jig.Integer);
          if (t == typeof(double)) return typeof(Jig.Float);
          if (t == typeof(string)) return typeof(Jig.String);                                                                                           
          throw new Exception($"unhandled type: {t}");
      }

    public ClrPrimitiveUnsafe Delegate {get;}
    
    
    public int Required {get;}
    public bool HasRest {get;}
    public override string Print() => "#<clr method>";
}