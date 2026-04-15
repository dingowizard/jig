using System.Linq.Expressions;
using System.Reflection;
using Jig;
using Jig.Types;
using Expression = System.Linq.Expressions.Expression;
namespace VM;

public delegate Jig.SchemeValue ClrPrimitiveUnsafe(Jig.List args);
public class ClrPrimitive : SchemeValue, ICallable {
    
    // TODO: It's a bit odd that this does not derive from Primitive. We have too many
    // different kinds of builtin procedures

    public ClrPrimitive(MethodBase mb, TypeResolver tr) {
        if (mb is MethodInfo mi) {
            if (mi.IsStatic) {
                var parameters = mi.GetParameters();                                                                                                        
                int count = parameters.Length;                          
                HasRest = parameters.Length > 0 &&                                                                                                           
                          parameters[^1].IsDefined(typeof(ParamArrayAttribute), false);
                Required = HasRest ? count - 1 : count;
                Delegate = MakeDelegate(mi, tr);
            } else {
                var parameters = mi.GetParameters();                                                                                                        
                int count = parameters.Length + 1;                          
                HasRest = parameters.Length > 0 &&                                                                                                           
                          parameters[^1].IsDefined(typeof(ParamArrayAttribute), false);
                Required = HasRest ? count - 1 : count;
                Delegate = MakeDelegate(mi, tr);
            }
        } else if (mb is ConstructorInfo ctor) {
            var parameters = mb.GetParameters();                                                                                                        
            int count = parameters.Length;                          
            HasRest = parameters.Length > 0 &&                                                                                                           
                      parameters[^1].IsDefined(typeof(ParamArrayAttribute), false);
            Required = HasRest ? count - 1 : count;
            Delegate = MakeDelegateForCtor(ctor, tr);
        } else {
            throw new NotImplementedException($"Unhandled MethodBase type {mb.GetType()}");
        }
    }
    
    private ClrPrimitiveUnsafe MakeDelegateForCtor(ConstructorInfo ctor, TypeResolver tr) {
        var listParam = Expression.Parameter(typeof(Jig.List), "args");                                                                                     
        var arrayVar = Expression.Variable(typeof(Jig.SchemeValue[]), "argsArray");                                                                            
        
        
        return Expression.Lambda<ClrPrimitiveUnsafe>(
            Expression.Block(
                [arrayVar],
                Expression.Assign(arrayVar, ArgListToArray(arrayVar, listParam)),
                MakeNewExpression(ctor, arrayVar, tr)),
            listParam).Compile();
    }
    private Expression MakeNewExpression(ConstructorInfo ctor, ParameterExpression arrayVar, TypeResolver tr) {
        return WrapReturn(
            ctor,
            Expression.New(ctor, MakeArgs(ctor, arrayVar, tr)),
            tr);
    }
    private ClrPrimitiveUnsafe MakeDelegate(MethodInfo mi, TypeResolver tr) {
        
        
        var listParam = Expression.Parameter(typeof(Jig.List), "args");                                                                                     
        var arrayVar = Expression.Variable(typeof(Jig.SchemeValue[]), "argsArray");                                                                            
        
        
        return Expression.Lambda<ClrPrimitiveUnsafe>(
            Expression.Block(
                [arrayVar],
                Expression.Assign(arrayVar, ArgListToArray(arrayVar, listParam)),
                MakeCallExpression(mi, arrayVar, tr)),
            listParam).Compile();
                                                          
    }

    private static Expression ArgListToArray(ParameterExpression arrayParam, ParameterExpression listParam) {
        return Expression.Call(
            typeof(Enumerable)
                .GetMethod("ToArray")!
                .MakeGenericMethod(typeof(Jig.SchemeValue)),
            listParam);
    }

    private static Expression MakeCallExpression(MethodInfo mi, ParameterExpression arrayArgsParam, TypeResolver tr) {

        if (mi.IsStatic) {
            return WrapReturn(
                mi,
                Expression.Call(mi, MakeArgs(mi, arrayArgsParam, tr)),
                tr);
        }
        var args = MakeArgs(mi, arrayArgsParam, tr);
        var call = Expression.Call(args[0], mi, args.Skip(1).ToArray());
        return WrapReturn(
            mi,
            call,
            tr);
    }

    private static Expression[] MakeArgs(MethodInfo mi, ParameterExpression args , TypeResolver tr) {
        if (mi.IsStatic) {
            var parameters = mi.GetParameters();
            System.Collections.Generic.List<Expression> argsList = [];
            int i = 0;
            foreach (var p in parameters) {
                // argsList.Add(WrapParameter(args, i, p));
                argsList.Add(ConvertArgExpr(args, i, p, tr));
                i++;

            }
            return argsList.ToArray();
        }
        
        System.Collections.Generic.List<Expression> xs = [ConvertObjectArgExpr(args, mi, tr)];
        var ps = mi.GetParameters();
        var n = 1;
        foreach (var p in ps) {
            // argsList.Add(WrapParameter(args, i, p));
            xs.Add(ConvertArgExpr(args, n, p, tr));
            n++;

        }
        return xs.ToArray();
    }

    private static Expression[] MakeArgs(ConstructorInfo ctor, ParameterExpression args , TypeResolver tr) {
        var parameters = ctor.GetParameters();
        System.Collections.Generic.List<Expression> argsList = [];
        int i = 0;
        foreach (var p in parameters) {
            // argsList.Add(WrapParameter(args, i, p));
            argsList.Add(ConvertArgExpr(args, i, p, tr));
            i++;

        }
        return argsList.ToArray();
    }
    private static Expression ConvertObjectArgExpr(ParameterExpression argsArrayParam, MethodInfo mi, TypeResolver tr) {
        
        Jig.Types.TypeDescriptor desc = tr.Resolve(mi.ReflectedType);
        var call = Expression.Invoke(Expression.Constant(desc.ConvertArg), Expression.ArrayIndex(argsArrayParam, Expression.Constant(0)));
        return Expression.Convert(call, mi.DeclaringType);
    }

    private static Expression ConvertArgExpr(ParameterExpression argsArrayParam, int argIndex, ParameterInfo p, TypeResolver tr) {

        Jig.Types.TypeDescriptor desc = tr.Resolve(p.ParameterType);
        var call = Expression.Invoke(Expression.Constant(desc.ConvertArg), Expression.ArrayIndex(argsArrayParam, Expression.Constant(argIndex)));
        return Expression.Convert(call, p.ParameterType);


    }


    private static Expression WrapReturn(MethodInfo mi, Expression expr, TypeResolver tr) {
        
        return Expression.Invoke(Expression.Constant(tr.Resolve(mi.ReturnType).WrapReturn), Expression.Convert(expr, typeof(object)));
        
    }

    private static Expression WrapReturn(ConstructorInfo ctor, Expression expr, TypeResolver tr) {
        
        return Expression.Invoke(Expression.Constant(tr.Resolve(ctor.DeclaringType).WrapReturn), Expression.Convert(expr, typeof(object)));
        
    }
    public ClrPrimitiveUnsafe Delegate {get;}
    
    
    public int Required {get;}
    public bool HasRest {get;}
    public override string Print() => "#<clr method>";
}