using System.Linq.Expressions;
using System.Reflection;

namespace Jig;
public class Continuation(Delegate d) : Procedure(d)
{
    public Thunk? Apply(List args) {
        switch (Value) {
            case ContinuationAny cAny:
                return  cAny(args.ToArray());
            case ListContinuation lc:
                return lc(args);
            case PairContinuation pc:
                return pc(args.ElementAt(0),  List.NewList(args.Skip(1).ToArray()));
            case ImproperListContinuation2 ic2:
                return ic2(args.ElementAt(0), args.ElementAt(1), List.NewList(args.Skip(2).ToArray()));
            case ImproperListContinuation3 ic3:
                return ic3(args.ElementAt(0), args.ElementAt(1), args.ElementAt(2), List.NewList(args.Skip(3).ToArray()));
            case ImproperListContinuation4 ic4:
                return ic4(args.ElementAt(0), args.ElementAt(1), args.ElementAt(2), args.ElementAt(3), List.NewList(args.Skip(4).ToArray()));
            case ImproperListContinuation5 ic5:
                return ic5(args.ElementAt(0), args.ElementAt(1), args.ElementAt(2), args.ElementAt(3), args.ElementAt(4), List.NewList(args.Skip(5).ToArray()));
            case ImproperListContinuation6 ic6:
                return ic6(args.ElementAt(0), args.ElementAt(1), args.ElementAt(2), args.ElementAt(3), args.ElementAt(4), args.ElementAt(5), List.NewList(args.Skip(6).ToArray()));
            case ImproperListContinuation7 ic7:
                return ic7(args.ElementAt(0), args.ElementAt(1), args.ElementAt(2), args.ElementAt(3), args.ElementAt(4), args.ElementAt(5), args.ElementAt(6), List.NewList(args.Skip(7).ToArray()));
            default:
                return  (Thunk?) Value.DynamicInvoke(args.ToArray<object?>());
        }

    }


    internal static Thunk? ApplyDelegate(Delegate k, IForm arg) {
        // TODO: should we handle more continuation types here?
            if (k is Continuation.ContinuationAny cany) {
                return cany(arg);
            }
            if (k is Continuation.OneArgDelegate c1) {
                return c1(arg);
            }
            return (Thunk?)k.DynamicInvoke(arg);
    }


    public override string Print() => "#<continuation>";

    public static Thunk call_with_values(Delegate k, Procedure producerExpr, Procedure consumerExpr) {
        // TODO: make signature consistent with other builtins. do validation of args
        var producer = producerExpr.Value; // the producer is a thunk, but internally that is something like (lambda (k) ...)
        var consumer = consumerExpr.Value;
        Delegate cont = ContinuationFromProc(k, consumer);
        Func<Delegate, Thunk> action = producer as Func<Delegate,Thunk> ?? throw new Exception("call-with-values: expected first argument to be a thunk.");
        return action(cont);
    }

    private static readonly MethodInfo _listFromEnumerableMethod = typeof(List).GetMethod("ListFromEnumerable") ?? throw new Exception("couldn't find ListFromEnumerable");

    private static Delegate ContinuationFromProc(Delegate k, Delegate consumerDel) {
        MethodInfo method = consumerDel.GetType().GetMethod("Invoke") ?? throw new Exception($"ContinuationFromProc: could not find 'Invoke' method on type of proc (proc.GetType())");
        var parameterInfos = method.GetParameters().Skip(1); // get parameters that are not the continuation
        var paramList = new System.Collections.Generic.List<ParameterExpression>();
        foreach (var p in parameterInfos)
        {
            paramList.Add(Expression.Parameter(typeof(IForm), p.ToString()));
        }
        Type? type = GetTypeForContinuation(consumerDel);
        if (type is null) {
            LambdaExpression lexpr = Expression.Lambda(
                body: Expression.Convert(ET.DynInv(new Expression [] {Expression.Constant(consumerDel), Expression.Constant(k)}.Concat(paramList).ToArray()), typeof(Thunk)),
                parameters: paramList.ToArray()
            );
            return lexpr.Compile();
        }
        if (type == typeof(ContinuationAny)) {
            ParameterExpression xs = Expression.Parameter(typeof(IForm[]));
            return Expression.Lambda<ContinuationAny>(
                body: Expression.Convert(ET.DynInv(new Expression [] {
                            Expression.Constant(consumerDel),
                            Expression.Constant(k),
                                Expression.Convert(
                                    expression: Expression.Call(
                                        method: _listFromEnumerableMethod,
                                        xs
                                        ),
                                    type: typeof(List))
                        }.ToArray()), typeof(Thunk)),
                parameters: new ParameterExpression[] {xs}
            ).Compile();
        }
        return Expression.Lambda(
            delegateType: type,
            body: Expression.Convert(ET.DynInv(new Expression [] {Expression.Constant(consumerDel), Expression.Constant(k)}.Concat(paramList).ToArray()), typeof(Thunk)),
            parameters: paramList.ToArray()
        ).Compile();
        throw new NotImplementedException();
    }

    private static Type? GetTypeForContinuation(Delegate proc)
    {
        return proc switch
        {
            ListFunction _ => typeof(ContinuationAny),
            PairFunction _ => typeof(PairContinuation),
            ImproperListFunction2 _ => typeof(ImproperListContinuation2),
            ImproperListFunction3 _ => typeof(ImproperListContinuation3),
            ImproperListFunction4 _ => typeof(ImproperListContinuation4),
            ImproperListFunction5 _ => typeof(ImproperListContinuation5),
            ImproperListFunction6 _ => typeof(ImproperListContinuation6),
            ImproperListFunction7 _ => typeof(ImproperListContinuation7),
            _ => null,
        };
    }


    public delegate Thunk? OneArgDelegate(IForm arg);
    public delegate Thunk? ContinuationAny(params IForm[] args);
    private delegate Thunk ListContinuation(List rest);
    private delegate Thunk PairContinuation(IForm arg0, List rest);
    private delegate Thunk ImproperListContinuation2(IForm arg0, IForm arg1, List rest);
    private delegate Thunk ImproperListContinuation3(IForm arg0, IForm arg1, IForm arg2, List rest);
    private delegate Thunk ImproperListContinuation4(IForm arg0, IForm arg1, IForm arg2, IForm arg3, List rest);
    private delegate Thunk ImproperListContinuation5(IForm arg0, IForm arg1, IForm arg2, IForm arg3, IForm arg4, List rest);
    private delegate Thunk ImproperListContinuation6(IForm arg0, IForm arg1, IForm arg2, IForm arg3, IForm arg4, IForm arg5, List rest);
    private delegate Thunk ImproperListContinuation7(IForm arg0, IForm arg1, IForm arg2, IForm arg3, IForm arg4, IForm arg5, IForm arg6, List rest);
}

