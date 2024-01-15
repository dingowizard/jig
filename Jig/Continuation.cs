using System.Linq.Expressions;
using System.Reflection;

namespace Jig;
public class Continuation : Procedure {

    public Continuation(Delegate d) : base (d) {}

    public Thunk Apply(List args) {
        switch (Value) {
            case ContinuationAny cany:
                return cany(args.ToArray());
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
                return  (Thunk) Value.DynamicInvoke(args.ToArray());
        }

    }


    internal static Thunk ApplyDelegate(Delegate k, Expr arg) {
        // TODO: should we handle more continuation types here?
            if (k is Continuation.ContinuationAny cany) {
                return cany(arg);
            }
            if (k is Continuation.OneArgDelegate c1) {
                return c1(arg);
            }
            return (Thunk)k.DynamicInvoke(arg);
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

    private static Delegate ContinuationFromProc(Delegate k, Delegate proc) {
        MethodInfo method = proc.GetType().GetMethod("Invoke") ?? throw new Exception($"ContinuationFromProc: could not find 'Invoke' method on type of proc (proc.GetType())");
        var parameterInfos = method.GetParameters().Skip(1); // get parameters that are not the continuation
        var paramList = new List<ParameterExpression>();
        foreach (var p in parameterInfos)
        {
            paramList.Add(Expression.Parameter(typeof(Expr), p.ToString()));
        }
        Type? type = GetTypeForContinuation(proc);
        if (type is null) {
            LambdaExpression lexpr = Expression.Lambda(
                body: Expression.Convert(ET.DynInv(new Expression [] {Expression.Constant(proc), Expression.Constant(k)}.Concat(paramList).ToArray()), typeof(Thunk)),
                parameters: paramList.ToArray()
            );
            return lexpr.Compile();
        }
        return Expression.Lambda(
            delegateType: type,
            body: Expression.Convert(ET.DynInv(new Expression [] {Expression.Constant(proc), Expression.Constant(k)}.Concat(paramList).ToArray()), typeof(Thunk)),
            parameters: paramList.ToArray()
        ).Compile();
        throw new NotImplementedException();
    }

    private static Type? GetTypeForContinuation(Delegate proc)
    {
        switch (proc) {
            case ListFunction _:
                return typeof(ListContinuation);
            case PairFunction _:
                return typeof(PairContinuation);
            case ImproperListFunction2 _:
                return typeof(ImproperListContinuation2);
            case ImproperListFunction3 _:
                return typeof(ImproperListContinuation3);
            case ImproperListFunction4 _:
                return typeof(ImproperListContinuation4);
            case ImproperListFunction5 _:
                return typeof(ImproperListContinuation5);
            case ImproperListFunction6 _:
                return typeof(ImproperListContinuation6);
            case ImproperListFunction7 _:
                return typeof(ImproperListContinuation7);
            default:
                return null;
        }
    }


    public delegate Thunk OneArgDelegate(Expr arg);
    public delegate Thunk ContinuationAny(params Expr[] args);
    private delegate Thunk ListContinuation(List rest);
    private delegate Thunk PairContinuation(Expr arg0, List rest);
    private delegate Thunk ImproperListContinuation2(Expr arg0, Expr arg1, List rest);
    private delegate Thunk ImproperListContinuation3(Expr arg0, Expr arg1, Expr arg2, List rest);
    private delegate Thunk ImproperListContinuation4(Expr arg0, Expr arg1, Expr arg2, Expr arg3, List rest);
    private delegate Thunk ImproperListContinuation5(Expr arg0, Expr arg1, Expr arg2, Expr arg3, Expr arg4, List rest);
    private delegate Thunk ImproperListContinuation6(Expr arg0, Expr arg1, Expr arg2, Expr arg3, Expr arg4, Expr arg5, List rest);
    private delegate Thunk ImproperListContinuation7(Expr arg0, Expr arg1, Expr arg2, Expr arg3, Expr arg4, Expr arg5, Expr arg6, List rest);
}

