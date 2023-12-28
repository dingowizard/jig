using System.Linq.Expressions;
using System.Reflection;

namespace Jig;
public class Continuation : Procedure {

    public Continuation(Delegate d) : base (d) {}

    public void Apply(List args) {
        switch (Value) {
            case ContinuationAny cany:
                cany(args.ToArray());
                return;
            case ListContinuation lc:
                lc(args);
                return;
            case PairContinuation pc:
                pc(args.ElementAt(0),  List.NewList(args.Skip(1).ToArray()));
                return;
            case ImproperListContinuation2 ic2:
                ic2(args.ElementAt(0), args.ElementAt(1), List.NewList(args.Skip(2).ToArray()));
                return;
            case ImproperListContinuation3 ic3:
                ic3(args.ElementAt(0), args.ElementAt(1), args.ElementAt(2), List.NewList(args.Skip(3).ToArray()));
                return;
            case ImproperListContinuation4 ic4:
                ic4(args.ElementAt(0), args.ElementAt(1), args.ElementAt(2), args.ElementAt(3), List.NewList(args.Skip(4).ToArray()));
                return;
            case ImproperListContinuation5 ic5:
                ic5(args.ElementAt(0), args.ElementAt(1), args.ElementAt(2), args.ElementAt(3), args.ElementAt(4), List.NewList(args.Skip(5).ToArray()));
                return;
            case ImproperListContinuation6 ic6:
                ic6(args.ElementAt(0), args.ElementAt(1), args.ElementAt(2), args.ElementAt(3), args.ElementAt(4), args.ElementAt(5), List.NewList(args.Skip(6).ToArray()));
                return;
            case ImproperListContinuation7 ic7:
                ic7(args.ElementAt(0), args.ElementAt(1), args.ElementAt(2), args.ElementAt(3), args.ElementAt(4), args.ElementAt(5), args.ElementAt(6), List.NewList(args.Skip(7).ToArray()));
                return;
            default:
                Value.DynamicInvoke(args.ToArray());
                return;
        }

    }

    internal static void ApplyDelegate(Delegate k, Expr arg) {
        // TODO: should we handle more continuation types here?
            if (k is Continuation.ContinuationAny cany) {
                cany(arg);
                return;
            }
            if (k is Continuation.OneArgDelegate c1) {
                c1(arg);
                return;
            }
            k.DynamicInvoke(arg);
            return;
    }


    public override string Print() => "#<continuation>";

    public static void call_with_values(Delegate k, Procedure producerExpr, Procedure consumerExpr) {
        // TODO: make signature consistent with other builtins. do validation of args
        var producer = producerExpr.Value; // the producer is a thunk, but internally that is something like (lambda (k) ...)
        var consumer = consumerExpr.Value;
        Delegate cont = ContinuationFromProc(k, consumer);
        Action<Delegate> action = producer as Action<Delegate> ?? throw new Exception("call-with-values: expected first argument to be a thunk.");
        action(cont);
        return;
    }

    private static Delegate ContinuationFromProc(Delegate k, Delegate proc) {
        MethodInfo method = proc.GetType().GetMethod("Invoke") ?? throw new Exception($"ContinuationFromProc: could not find 'Invoke' method on type of proc (proc.GetType())");
        var parameterInfos = method.GetParameters().Skip(1); // get parameters that are not the continuation
        var paramList = new List<ParameterExpression>();
        foreach (var p in parameterInfos)
        {
            paramList.Add(Expression.Parameter(typeof(Expr), p.ToString()));
        }
        // TODO: handle situation when lambda expression has a rest param Eg (lambda (a . rest) rest) or (lambda l l)
        Type? type = GetTypeForContinuation(proc);
        if (type is null) {
            LambdaExpression lexpr = Expression.Lambda(
                body: ET.DynInv(new Expression [] {Expression.Constant(proc), Expression.Constant(k)}.Concat(paramList).ToArray()),
                parameters: paramList.ToArray()
            );
            return lexpr.Compile();
        }
        return Expression.Lambda(
            delegateType: type,
            body: ET.DynInv(new Expression [] {Expression.Constant(proc), Expression.Constant(k)}.Concat(paramList).ToArray()),
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


    public delegate void OneArgDelegate(Expr arg);
    public delegate void ContinuationAny(params Expr[] args);
    private delegate void ListContinuation(List rest);
    private delegate void PairContinuation(Expr arg0, List rest);
    private delegate void ImproperListContinuation2(Expr arg0, Expr arg1, List rest);
    private delegate void ImproperListContinuation3(Expr arg0, Expr arg1, Expr arg2, List rest);
    private delegate void ImproperListContinuation4(Expr arg0, Expr arg1, Expr arg2, Expr arg3, List rest);
    private delegate void ImproperListContinuation5(Expr arg0, Expr arg1, Expr arg2, Expr arg3, Expr arg4, List rest);
    private delegate void ImproperListContinuation6(Expr arg0, Expr arg1, Expr arg2, Expr arg3, Expr arg4, Expr arg5, List rest);
    private delegate void ImproperListContinuation7(Expr arg0, Expr arg1, Expr arg2, Expr arg3, Expr arg4, Expr arg5, Expr arg6, List rest);
}
