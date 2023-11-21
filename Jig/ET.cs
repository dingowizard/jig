using System.Dynamic;
using System.Linq.Expressions;
using System.Reflection;
using Microsoft.Scripting.Actions;

namespace Jig;

internal delegate void ListFunction(Continuation k, List rest);
internal delegate void PairFunction(Continuation k, Expr arg0, List rest);
internal delegate void ImproperListFunction2(Continuation k, Expr arg0, Expr arg1, List rest);
internal delegate void ImproperListFunction3(Continuation k, Expr arg0, Expr arg1, Expr arg2, List rest);
internal delegate void ImproperListFunction4(Continuation k, Expr arg0, Expr arg1, Expr arg2, Expr arg3, List rest);
internal delegate void ImproperListFunction5(Continuation k, Expr arg0, Expr arg1, Expr arg2, Expr arg3, Expr arg4, List rest);
internal delegate void ImproperListFunction6(Continuation k, Expr arg0, Expr arg1, Expr arg2, Expr arg3, Expr arg4, Expr arg5, List rest);
internal delegate void ImproperListFunction7(Continuation k, Expr arg0, Expr arg1, Expr arg2, Expr arg3, Expr arg4, Expr arg5, Expr arg6, List rest);

internal abstract class ET : Expression {

    public CompiledCode Compile() {
        return Expression.Lambda<CompiledCode>(Body, new ParameterExpression[] {kParam, envParam}).Compile();
    }

    protected ET() {
        kParam = Expression.Parameter(typeof(Continuation));
        envParam = Expression.Parameter(typeof(IEnvironment));
    }

    public static ET Analyze(LexicalContext scope, Expr ast) {
        switch (ast) {
            case Expr.Integer: case Expr.Double:
            case Expr.Boolean:
                return new LiteralET(ast);
            case Expr.Symbol symbol:
                return new SymbolET(scope, symbol);
            case List.NonEmptyList list:
                if (list.Car.Equals(  new Expr.Symbol("lambda"))) {
                    return new MakeProcET(scope, (List.NonEmptyList)list.Cdr);
                }
                if (list.Car.Equals(new Expr.Symbol("quote"))) {
                    return new LiteralET(((List.NonEmptyList)list.Cdr).Car);
                }
                return new ProcAppET(scope, list);
            default:
                throw new Exception($"Analyze: doesnn't know what to do with {ast}");
        }
    }

    internal static ET Analyze(LexicalContext lexVars, SyntaxObject stx) {
        if (stx is SyntaxObject.Literal lit) {
            return new LiteralET(lit);
        }
        if (stx is SyntaxObject.Identifier id) {
                return new SymbolET(lexVars, id);
        }
        Expr x = SyntaxObject.E(stx);
        if (x is IPair pair) {
            return MakeETForSyntaxPair(lexVars, pair);
        }

        throw new Exception($"Analyze: doesn't know what to do with {stx}");
    }

    public override bool CanReduce {get;} = true;

    public override Expression Reduce() {
        return Expression.Lambda<CompiledCode>(Body, new ParameterExpression[] {kParam, envParam});
    }

    public override ExpressionType NodeType {get;} = ExpressionType.Extension;

    public override Type Type {get;} = typeof(CompiledCode);

    // public static Expression Block(LexicalScope scope, ProperList properList) => new BlockET(scope, properList);

    public ParameterExpression kParam;
    public ParameterExpression envParam;
    public abstract Expression Body {get;}

    protected static Expression ConvertToObject(Expression x) => Expression.Convert(x, typeof(object)); // TODO: should this be ConvertToExpr?

    private class LiteralET : ET {

        public LiteralET(Expr x) : base() {
            Body = ConvertToObject(DynInv(kParam, Expression.Constant(x)));
            // Body = DynInv(kParam, Expression.Constant(x));
        }

        public LiteralET(SyntaxObject stx) : base () {
            Expr x = SyntaxObject.ToDatum(stx);
            Body = ConvertToObject(DynInv(kParam, Expression.Constant(x)));
        }

        public override Expression Body {get;}

    }

    private class SymbolET : ET {

        private static MethodInfo LookUp {get;} = typeof(IEnvironment).GetMethod("LookUp") ?? throw new Exception("in SymbolET: IEnvironment really should have a 'LookUp' method");
        private static MethodInfo LookUpSyntax {get;} = typeof(IEnvironment).GetMethod("LookUpSyntax") ?? throw new Exception("in SymbolET: IEnvironment really should have a 'LookUpSyntax' method");

        public SymbolET (LexicalContext scope, Expr.Symbol symbol) : base() {
            ParameterExpression? pe = scope.LookUp(symbol);
            if (pe is null) {
                Body = Expression.Call(envParam,
                                       LookUp,
                                       new Expression [] {kParam, Expression.Constant(symbol)});
            } else {
                Body = Expression.Invoke(kParam, new Expression[] {pe});
            }
        }

        public SymbolET(LexicalContext lexVars, SyntaxObject.Identifier id) {
            ParameterExpression? pe = lexVars.LookUp(id.Symbol);
            if (pe is null) {
                Body = Expression.Call(envParam,
                                       LookUpSyntax,
                                       new Expression [] {kParam, Expression.Constant(id)});
            } else {
                Body = Expression.Invoke(kParam, new Expression[] {pe});
            }
        }

        public override Expression Body {get;}
    }

    internal delegate List MakeListDelegate(params CompiledCode[] args);

    delegate void MapInternalDelegate(Continuation continuation, Action<Continuation, LiteralExpr<CompiledCode>> proc, List list );

    delegate void ApplyDelegate(Continuation k, LiteralExpr<Delegate> proc, List args);
    static PropertyInfo carPropertyInfo = typeof(IPair).GetProperty("Car") ?? throw new Exception("in ProcAppET: ProperLists should have one property named 'Car'");
    static PropertyInfo cdrPropertyInfo = typeof(IPair).GetProperty("Cdr") ?? throw new Exception("in ProcAppET: ProperLists should have one property named 'Cdr'");

    private class ProcAppET : ET {

// (define (analyze-call x)
//   (let ((analyzed (map analyze/cps x)))
//     (lambda (k env)
//       (map/cps (lambda (v) (apply/cps k (car v) (cdr v))) (lambda (cont proc) (proc cont env)) analyzed))))
//

        public ProcAppET(LexicalContext scope, List.NonEmptyList list) : base () {
            // TODO: probably there is a more certain way of checking to see that we have syntax pair?
            IEnumerable<Expression<CompiledCode>> analyzed =
                list.Car is SyntaxObject ?
                list.Select(x => (Expression<CompiledCode>)Analyze(scope, (SyntaxObject)x).Reduce()) :
                list.Select(x => (Expression<CompiledCode>)Analyze(scope, x).Reduce());
            var vParam = Expression.Parameter(typeof(Expr));
            var contParam = Expression.Parameter(typeof(Continuation));
            var procParam = Expression.Parameter(typeof(LiteralExpr<CompiledCode>));
            MakeListDelegate listProc = List.NewListFromObjects;
            Body = ConvertToObject(
                DynInv(Expression.Constant((MapInternalDelegate) Builtins.map_internal),
                       Expression.Lambda<Continuation>(
                           body: DynInv(Expression.Constant((ApplyDelegate)Builtins.apply),
                                        kParam,
                                        Expression.Property(Expression.Convert(vParam, typeof(IPair)), carPropertyInfo),
                                        // DynInv(Expression.Constant(car), vParam),
                                        Expression.Property(Expression.Convert(vParam, typeof(IPair)), cdrPropertyInfo)),
                                        // DynInv(Expression.Constant(cdr), vParam)),
                           parameters: new ParameterExpression[] {vParam}),
                       Expression.Lambda<Action<Continuation, LiteralExpr<CompiledCode>>>(
                           body: Expression.Invoke(Expression.Property(procParam, "Value"),  new Expression[] {contParam, envParam}),
                           parameters: new ParameterExpression[] {contParam, procParam}),
                       Expression.Invoke(Expression.Constant(listProc), Expression.NewArrayInit(typeof(CompiledCode), analyzed))));
        }

        public override Expression Body {get;}
    }

    private Expression<CompiledCode> LE(Expression body, ParameterExpression[] ps) => Expression.Lambda<CompiledCode> (body, ps);
    private Expression<Continuation> K(Expression body, ParameterExpression[] ps) => Expression.Lambda<Continuation> (body, ps);
    private Expression DynInv(params Expression[] xs) {
        return Expression.Dynamic(
            binder: new MyInvokeBinder(new CallInfo(xs.Length -1)),
            returnType: typeof(object),
            arguments: xs);
    }

    internal static ET MakeETForSyntaxPair(LexicalContext lexVars, IPair stxPair) {
        SyntaxObject car = stxPair.Car as SyntaxObject ?? throw new Exception($"in MakeETForSyntaxPair: expected car to be a syntax object but got {stxPair.Car}" );
        List args = stxPair.Cdr as List ?? throw new Exception($"in MakeETForSyntaxPair: expected cdr to be a list, but got {stxPair.Cdr}");
        if (car is SyntaxObject.Identifier id) {
            switch (id.Symbol.Name) {
                case "quote":
                    SyntaxObject quotedStx = args.ElementAt(0) as SyntaxObject ?? throw new Exception($"expected syntax object");
                    return new LiteralET(quotedStx);
                case "lambda":
                    List.NonEmptyList nonEmptyArgs = args as List.NonEmptyList ?? throw new Exception($"malformed lambda expression {stxPair}");
                    return new MakeProcET(lexVars, nonEmptyArgs);

                default:
                    if (stxPair is List.NonEmptyList list) {
                        return new ProcAppET(lexVars, list);
                    } else {
                        throw new Exception($"in MakeETForSyntaxPair: unhandled {stxPair}");
                           }
            }
        }
        throw new NotImplementedException();
    }

    private class MakeProcET : ET {

        public MakeProcET(LexicalContext scope, List.NonEmptyList args) {

            Expr lambdaParameters = args.Car;
            List.NonEmptyList lambdaBody = args.Cdr as List.NonEmptyList ?? throw new Exception($"malformed lambda: no body");

            var k = Expression.Parameter(typeof(Continuation), "k in MakeProcET"); // this is the continuation paramter for the proc we are making
            LexicalContext lambdaScope;
            ConstructorInfo constructor = typeof(LiteralExpr<Delegate>).GetConstructor(new Type[] {typeof(Delegate)}) ?? throw new Exception("could not find constructor for LiteralExpr<Delegate>");
            switch (lambdaParameters) {
                case List.NonEmptyList properList:
                    IEnumerable<Expr.Symbol> properListSymbols = properList.Cast<Expr.Symbol>();
                    if (properListSymbols is null) throw new Exception($"malformed lambda: expected parameters to be symbols but got {properList}");
                    lambdaScope = scope.Extend(properListSymbols);
                    Body = Expression.Invoke(kParam, // here kParam is the continuation when the lambda expression is being evaluated
                                             Expression.New(constructor,
                                                            new Expression[] {
                                                                Expression.Lambda(body: LambdaBody(k, lambdaScope, lambdaBody),
                                                                                  parameters: new ParameterExpression[] {k}.Concat(lambdaScope.Parameters))
                                                            }));
                    return;
                // case SyntaxObject.Identifier id:
                case Expr.Symbol onlyRestSymbol: // cases like (lambda x x)
                    lambdaScope = scope.Extend(new Expr.Symbol[]{onlyRestSymbol});

                    Body = Expression.Invoke(kParam, // here kParam is the continuation when the lambda expression is being evaluated
                                             Expression.New(constructor,
                                                            new Expression[] {
                                                                Expression.Lambda(delegateType: typeof(ListFunction),
                                                               body: LambdaBody(k, lambdaScope, lambdaBody),
                                                               parameters: new ParameterExpression[] {k}.Concat(lambdaScope.Parameters))
                                                            }));
                    return;
                case IPair andRest: // cases like (lambda (proc l . ls))
                    IEnumerable<Expr.Symbol> symbols = ValidateAndConvertToSymbols(andRest);
                    lambdaScope = scope.Extend(symbols);
                    Body = Expression.Invoke(kParam, // here kParam is the continuation when the lambda expression is being evaluated
                                             Expression.New(constructor,
                                                            new Expression[] {
                                                                Expression.Lambda(delegateType: FunctionTypeFromParameters(andRest),
                                                               body: LambdaBody(k, lambdaScope, lambdaBody),
                                                               parameters: new ParameterExpression[] {k}.Concat(lambdaScope.Parameters))
                                                            }));
                    return;

                default:
                    throw new Exception($"in MakeProcET: can't handle parameters with type {lambdaParameters.GetType()} yet.");
            }

        }

        public override Expression Body {get;}

        private IEnumerable<Expr.Symbol> ValidateAndConvertToSymbols(IPair parameters) {
            var result = new List<Expr.Symbol>();
            object car = parameters.Car;
            Expr.Symbol sym = car as Expr.Symbol ?? throw new Exception($"lambda: all parameters must be symbols (given {car})");
            result.Add(sym);
            object cdr = parameters.Cdr;
            while (cdr is IPair pairCdr) {
                car = pairCdr.Car;
                sym = car as Expr.Symbol ?? throw new Exception($"lambda: all paramters must be symbols (given {car})");
                result.Add(sym);
                cdr = pairCdr.Cdr;
            }
            sym = cdr as Expr.Symbol ?? throw new Exception($"lambda: all paramters must be symbols (given {cdr})");
            result.Add(sym);
            return result;

        }

        private Type FunctionTypeFromParameters(IPair parameters) {
            // how many parameters before rest parameter?
            int num = 1;
            object cdr = parameters.Cdr;
            while (cdr is IPair pairCdr) {
                num ++;
                cdr = pairCdr.Cdr;
            }
            switch (num) {
                case 0:
                    throw new Exception("In FunctionTypeFromParameters: found 0 parameters before the rest paramter, but there should be at least one");
                case 1:
                    return typeof(PairFunction);
                case 2:
                    return typeof(ImproperListFunction2);
                case 3:
                    return typeof(ImproperListFunction3);
                case 4:
                    return typeof(ImproperListFunction4);
                case 5:
                    return typeof(ImproperListFunction5);
                case 6:
                    return typeof(ImproperListFunction6);
                case 7:
                    return typeof(ImproperListFunction7);
                default:
                    throw new Exception("lambda: can't handle improper list parameters with more than 7 parameters before the rest paramter.");
            }
        }

        private Expression LambdaBody(ParameterExpression k, LexicalContext scope, List.NonEmptyList exprs) {
            // k is the paramter continuation for the procedure that is being made (the continuation when that proc is applied)
            var block = new BlockET(scope.Extend(), exprs);
            var cont = Expression.Parameter(typeof(Continuation)); // this is the continuation parameter for the inner lambda

            // this should represent what code to run when the procedure is called
            return Expression.Invoke(
                Expression.Lambda<Action<Continuation>>(
                    // body: DynInv(new BlockET(scope.Extend(), exprs).LambdaExpression(), cont, envParam),
                    body: Expression.Invoke(
                        // Analyze(scope, exprs.ElementAt(0)),
                        block,
                        new Expression[] {cont, envParam}),
                    parameters: new ParameterExpression[] {cont}),
                new Expression[] {k}
            );
        }

        private static Type GetImproperListFunctionType(int v) {
            switch (v) {
                case 0:
                case 1:
                    throw new Exception("In GetImproperListFunctionType: number of required parameters should be more than 1");
                case 2:
                    return typeof(ImproperListFunction2);
                case 3:
                    return typeof(ImproperListFunction3);
                case 4:
                    return typeof(ImproperListFunction4);
                case 5:
                    return typeof(ImproperListFunction5);
                case 6:
                    return typeof(ImproperListFunction6);
                case 7:
                    return typeof(ImproperListFunction7);
                default:
                    throw new NotImplementedException("Improper parameter lists are limited to 8 parameters");
            }
        }
    }
}

internal class BlockET : ET {

    public BlockET(LexicalContext scope, List.NonEmptyList exprs)
    {


        MakeListDelegate listProc = List.NewListFromObjects;
        IEnumerable<Expression<CompiledCode>> analyzed = exprs.Select(x => (Expression<CompiledCode>)Analyze(scope, x).Reduce());
        var listExpr = Expression.Convert(Expression.Invoke(Expression.Constant(listProc), Expression.NewArrayInit(typeof(CompiledCode), analyzed)), typeof(List.NonEmptyList));

                       // Expression.Invoke(Expression.Constant(listProc), Expression.NewArrayInit(typeof(CompiledCode), analyzed))));
        Body = Expression.Invoke(
            Expression.Constant((Action<Continuation, IEnvironment, List.NonEmptyList>) doSequence),
            new Expression[] {
                kParam,
                envParam,
                listExpr
            }
        );

        // Body = Expression.Block(exprs.Select(x => Compiler.Analyze(scope, x)));

    }

    private void doSequence(Continuation k, IEnvironment env, List list) {
        // TODO: make caller guarantee properlist
        List.NonEmptyList? exprs = list as List.NonEmptyList;
        if (exprs == null) throw new Exception("doSequence: should not be called with empty list");
        var code = exprs.Car as LiteralExpr<CompiledCode> ?? throw new Exception($"BlockET.doSequence: expected a CompiledCode but got {exprs.Car.GetType()}");
        List cdr = (List)exprs.Cdr;
        Continuation cont = cdr is Expr.NullType ? k : (v) => doSequence(k,env,(List.NonEmptyList)cdr);
        code.Value(cont, env);
    }

    public override Expression Body {get;}
}

public class MyInvokeMemberBinder : InvokeMemberBinder {

    public MyInvokeMemberBinder(string name, bool ignoreCase, CallInfo callInfo ) : base(name, ignoreCase, callInfo) {}

    public override DynamicMetaObject FallbackInvokeMember(DynamicMetaObject target, DynamicMetaObject[] args, DynamicMetaObject? errorSuggestion) {
        var restrictions = GetTargetArgsRestrictions(target, args, false);
        // TODO: make this handle more complicated cases. Look at Sympl for more help
        MethodInfo method = target.LimitType.GetMethod(this.Name) ?? throw new Exception("in FallbackInvokeMember: couldn't find methodInfo");
        return new DynamicMetaObject(
                EnsureObjectResult(
                    Expression.Call(
                    Expression.Convert(target.Expression,
                                        target.LimitType),
                    method, args.Select(mo => mo.Expression))),
                restrictions);
    }

    public static Expression EnsureObjectResult (Expression expr) {
        if (! expr.Type.IsValueType)
            return expr;
        if (expr.Type == typeof(void))
            return Expression.Block(
                        expr, Expression.Default(typeof(object)));
        else
            return Expression.Convert(expr, typeof(object));
    }
    public override DynamicMetaObject FallbackInvoke(DynamicMetaObject target, DynamicMetaObject[] args, DynamicMetaObject? errorSuggestion) {
        throw new NotImplementedException();
    }

    internal static CallSignature CallInfoToSignature(CallInfo callInfo) {
        Argument[] ai = new Argument[callInfo.ArgumentCount];
        int positionalArgNum = callInfo.ArgumentCount - callInfo.ArgumentNames.Count;
        int i;
        for (i = 0; i < positionalArgNum; i++) {
            ai[i] = new Argument(ArgumentType.Simple);
        }
        foreach (var name in callInfo.ArgumentNames) {
            ai[i++] = new Argument(
                ArgumentType.Named,
                name
            );
        }
        return new CallSignature(ai);
    }
    public static BindingRestrictions GetTargetArgsRestrictions(
            DynamicMetaObject target, DynamicMetaObject[] args,
            bool instanceRestrictionOnTarget){
        // Important to add existing restriction first because the
        // DynamicMetaObjects (and possibly values) we're looking at depend
        // on the pre-existing restrictions holding true.
        var restrictions = target.Restrictions.Merge(BindingRestrictions
                                                        .Combine(args));
        if (instanceRestrictionOnTarget) {
            restrictions = restrictions.Merge(
                BindingRestrictions.GetInstanceRestriction(
                    target.Expression,
                    target.Value
                ));
        } else {
            restrictions = restrictions.Merge(
                BindingRestrictions.GetTypeRestriction(
                    target.Expression,
                    target.LimitType
                ));
        }
        for (int i = 0; i < args.Length; i++) {
            BindingRestrictions r;
            if (args[i].HasValue && args[i].Value == null) {
                r = BindingRestrictions.GetInstanceRestriction(
                        args[i].Expression, null);
            } else {
                r = BindingRestrictions.GetTypeRestriction(
                        args[i].Expression, args[i].LimitType);
            }
            restrictions = restrictions.Merge(r);
        }
        return restrictions;
    }

}
public class MyInvokeBinder : InvokeBinder {

    public MyInvokeBinder(CallInfo callInfo) : base(callInfo) {}

    public override DynamicMetaObject FallbackInvoke(DynamicMetaObject target, DynamicMetaObject[] args, DynamicMetaObject? errorSuggestion) {
        return new DefaultBinder().Call(CallInfoToSignature(this.CallInfo), target, args);
    }

    internal static CallSignature CallInfoToSignature(CallInfo callInfo) {
        Argument[] ai = new Argument[callInfo.ArgumentCount];
        int positionalArgNum = callInfo.ArgumentCount - callInfo.ArgumentNames.Count;
        int i;
        for (i = 0; i < positionalArgNum; i++) {
            ai[i] = new Argument(ArgumentType.Simple);
        }
        foreach (var name in callInfo.ArgumentNames) {
            ai[i++] = new Argument(
                ArgumentType.Named,
                name
            );
        }
        return new CallSignature(ai);
    }
}
