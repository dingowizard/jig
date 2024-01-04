using System.Dynamic;
using System.Linq.Expressions;
using System.Reflection;
using Microsoft.Scripting.Actions;

namespace Jig;

internal delegate void ListFunction(Delegate k, List rest);
internal delegate void PairFunction(Delegate k, Expr arg0, List rest);
internal delegate void ImproperListFunction2(Delegate k, Expr arg0, Expr arg1, List rest);
internal delegate void ImproperListFunction3(Delegate k, Expr arg0, Expr arg1, Expr arg2, List rest);
internal delegate void ImproperListFunction4(Delegate k, Expr arg0, Expr arg1, Expr arg2, Expr arg3, List rest);
internal delegate void ImproperListFunction5(Delegate k, Expr arg0, Expr arg1, Expr arg2, Expr arg3, Expr arg4, List rest);
internal delegate void ImproperListFunction6(Delegate k, Expr arg0, Expr arg1, Expr arg2, Expr arg3, Expr arg4, Expr arg5, List rest);
internal delegate void ImproperListFunction7(Delegate k, Expr arg0, Expr arg1, Expr arg2, Expr arg3, Expr arg4, Expr arg5, Expr arg6, List rest);

internal abstract class ET : Expression {

    public CompiledCode Compile() {
        return Expression.Lambda<CompiledCode>(Body, new ParameterExpression[] {kParam, envParam}).Compile();
    }

    protected ET() {
        kParam = Expression.Parameter(typeof(Delegate));
        envParam = Expression.Parameter(typeof(IEnvironment));
    }

    public static ET Analyze(LexicalContext scope, Expr ast) {
        if (Expr.IsLiteral(ast)) {
            return new LiteralET(ast);
        } else if (Expr.IsSymbol(ast)) {
            return new SymbolET(scope, ast);
        } else if (Expr.IsNonEmptyList(ast)) {
            List.NonEmpty list = ast is SyntaxObject stx ? (List.NonEmpty)SyntaxObject.E(stx) : (List.NonEmpty)ast;
            // TODO: rewrite below ET constructors to take full ast (as list probably)
            if (Expr.IsKeyword("quote", ast)) {
                // TODO: QuoteET
                return new LiteralET(((List.NonEmpty)list.Cdr).Car);
            } else if (Expr.IsKeyword("lambda", ast)) {
                return new LambdaExprET(scope, (List.NonEmpty)list.Cdr);
            } else if (Expr.IsKeyword("if", ast)) {
                return new IfET(scope, list);
            } else if (Expr.IsKeyword("define", ast)) {
                return new DefineET(scope, list);
            } else if (Expr.IsKeyword("begin", ast)){
                return new BlockET(scope, (List.NonEmpty)list.Cdr);
            }
            else if (Expr.IsKeyword("set!", ast)){
                return new SetBangET(scope, list);
            }
            else {
                return new ProcAppET(scope, list);
            }
        } else {
            throw new Exception($"Analyze: doesnn't know what to do with {ast}");
        }
    }

    public override bool CanReduce {get;} = true;

    public override Expression Reduce() {
        return Expression.Lambda<CompiledCode>(Body, new ParameterExpression[] {kParam, envParam});
    }

    public override ExpressionType NodeType {get;} = ExpressionType.Extension;

    public override Type Type {get;} = typeof(CompiledCode);

    public ParameterExpression kParam;
    public ParameterExpression envParam;
    public abstract Expression Body {get;}

    private class LiteralET : ET {

        public LiteralET(Expr x) : base() {
            x = x is SyntaxObject stx ? SyntaxObject.ToDatum(stx) : x;
            Body = DynInv(kParam, Expression.Constant(x));
        }

        public override Expression Body {get;}

    }

    private class SymbolET : ET {

        private static MethodInfo LookUp {get;} = typeof(IEnvironment).GetMethod("LookUp") ?? throw new Exception("in SymbolET: IEnvironment really should have a 'LookUp' method");

        public SymbolET (LexicalContext scope, Expr x) {
            ParameterExpression? pe = scope.LookUp(x);
            if (pe is null) {
                var v = Expression.Parameter(typeof(Expr));
                var k = Expression.Lambda<Continuation.OneArgDelegate>(DynInv(kParam, v), new ParameterExpression[] {v});
                Body = Expression.Call(envParam,
                                       LookUp,
                                       new Expression [] {k, Expression.Constant(x)});
            } else {
                Body = DynInv(kParam, pe);
            }
        }

        public override Expression Body {get;}
    }

    internal delegate List MakeListDelegate(params CompiledCode[] args);

    delegate void MapInternalDelegate(Continuation.OneArgDelegate continuation, Action<Continuation.OneArgDelegate, LiteralExpr<CompiledCode>> proc, List list );

    delegate void ApplyDelegate(Delegate k, Procedure proc, List args);
    static PropertyInfo carPropertyInfo = typeof(IPair).GetProperty("Car") ?? throw new Exception("in ProcAppET: ProperLists should have one property named 'Car'");
    static PropertyInfo cdrPropertyInfo = typeof(IPair).GetProperty("Cdr") ?? throw new Exception("in ProcAppET: ProperLists should have one property named 'Cdr'");


    private class ProcAppET : ET {

        public ProcAppET(LexicalContext scope, List.NonEmpty list) : base () {
            // TODO: probably there is a more certain way of checking to see that we have syntax pair?
            IEnumerable<Expression<CompiledCode>> analyzed =
                list.Select(x => (Expression<CompiledCode>)Analyze(scope, x).Reduce());
            var vParam = Expression.Parameter(typeof(Expr));
            var v = Expression.Parameter(typeof(Expr));
            var contParam = Expression.Parameter(typeof(Continuation.OneArgDelegate));
            var procParam = Expression.Parameter(typeof(LiteralExpr<CompiledCode>));
            MakeListDelegate listProc = List.NewListFromObjects;
            var k = Expression.Lambda<Continuation.OneArgDelegate>(
                            DynInv(Expression.Constant((ApplyDelegate)Builtins.apply),
                                   kParam,
                                   Expression.Property(Expression.Convert(vParam, typeof(IPair)), carPropertyInfo),
                                   Expression.Property(Expression.Convert(vParam, typeof(IPair)), cdrPropertyInfo)),
                           parameters: new ParameterExpression[] {vParam});
            Body =
                DynInv(Expression.Constant((MapInternalDelegate) Builtins.map_internal),
                       k,
                       Expression.Lambda<Action<Continuation.OneArgDelegate, LiteralExpr<CompiledCode>>>( // (lambda (k code) (code k env))
                           body: DynInv(Expression.Property(procParam, "Value"), contParam, envParam),
                           parameters: new ParameterExpression[] {contParam, procParam}),
                       Expression.Invoke(Expression.Constant(listProc), Expression.NewArrayInit(typeof(CompiledCode), analyzed)));
        }

        public override Expression Body {get;}
    }

    private class IfET : ET {

        public IfET(LexicalContext lexVars, List.NonEmpty list) : base() {

            List.NonEmpty listCdr = list.Cdr as List.NonEmpty ?? throw new Exception($"malformed if: {list}"); // TODO: should the parser be doing all this checking for malformed whatevers?
            Expr cond = listCdr.Car;
            Expression<CompiledCode> condCC = (Expression<CompiledCode>)Analyze(lexVars, cond).Reduce();
            List.NonEmpty listCdrCdr = listCdr.Cdr as List.NonEmpty ?? throw new Exception($"malformed if: {list}");
            Expr consq = listCdrCdr.Car;
            Expression<CompiledCode> consqCC = (Expression<CompiledCode>)Analyze(lexVars, consq).Reduce();
            List.NonEmpty listCdrCdrCdr = listCdrCdr.Cdr as List.NonEmpty ?? throw new Exception($"malformed if: {list}");
            Expr alt = listCdrCdrCdr.Car;
            Expression<CompiledCode> altCC = (Expression<CompiledCode>)Analyze(lexVars, alt).Reduce();
            ParameterExpression boolResult = Expression.Parameter(typeof(Expr), "boolResult");
            var k0 = Expression.Lambda<Continuation.OneArgDelegate>(
                body: Expression.IfThenElse(
                    test: Expression.Convert(DynInv(Expression.Constant((Func<Expr, bool>) IfET.IsNotFalse), boolResult), typeof(bool)),
                    ifTrue: Expression.Invoke(consqCC, new Expression[] {kParam, envParam}),
                    ifFalse: Expression.Invoke(altCC, new Expression[] {kParam, envParam})),
                parameters: new ParameterExpression[] {boolResult});
            Body = Expression.Invoke(condCC, new Expression[] {k0, envParam});

        }

        public override Expression Body {get;}
        private static bool IsNotFalse(Expr x) {
            if (x is Expr.Boolean boolExpr) {
                return boolExpr.Value != false;
            }
            else return true;
        }
    }


    private class SetBangET : ET {

        private static MethodInfo _setMethod {get;} = typeof(IEnvironment).GetMethod("Set") ?? throw new Exception("while initializeing SetBangET, could not find 'Set' method on IEnvironment");

        public SetBangET(LexicalContext lexVars, List.NonEmpty list) : base() {
            Expr sym = list.ElementAt(1);
            Expr valExpr = list.ElementAt(2);
            Expression<CompiledCode> valCC = (Expression<CompiledCode>)Analyze(lexVars, valExpr).Reduce();
            ParameterExpression val = Expression.Parameter(typeof(Expr), "val");
            Expression contBody;
            ParameterExpression? pe = lexVars.LookUp(sym);
            if (pe is null) {
                contBody = Expression.Call(envParam,
                                       _setMethod,
                                       new Expression [] {kParam, Expression.Constant(sym), val});
            } else {
                contBody = DynInv(kParam, Expression.Assign(pe, val));
            }
            var k = Expression.Lambda(contBody, new ParameterExpression [] {val});

            Body = Expression.Invoke(valCC, new Expression[] {k, envParam});
        }

        public override Expression Body {get;}
    }

    private class DefineET : ET {

        private static MethodInfo _defineMethod {get;} = typeof(IEnvironment).GetMethod("Define") ?? throw new Exception("while initializing DefineET, could not find 'Define' method on IEnvironment");

        public DefineET(LexicalContext lexVars, List.NonEmpty list) : base() {
            Expr sym = list.ElementAt(1);
            Expr valExpr = list.ElementAt(2);
            Expression<CompiledCode> valCC = (Expression<CompiledCode>)Analyze(lexVars, valExpr).Reduce();
            ParameterExpression val = Expression.Parameter(typeof(Expr), "val");
            Expression contBody;
            if (lexVars.AtTopLevel()) {
                    // we're defining a variable at global scope
                    contBody = Expression.Call(envParam,
                                               _defineMethod,
                                               new Expression[] {kParam, Expression.Constant(sym), val});

            } else {
                ParameterExpression pe = lexVars.ParameterForDefine(sym);
                contBody = DynInv(kParam, Expression.Assign(pe, val));
            }


            var k = Expression.Lambda(contBody, new ParameterExpression [] {val});

            Body = Expression.Invoke(valCC, new Expression[] {k, envParam});

        }


        public override Expression Body {get;}
    }

    private Expression<CompiledCode> LE(Expression body, ParameterExpression[] ps) => Expression.Lambda<CompiledCode> (body, ps);
    private Expression<Continuation.OneArgDelegate> K(Expression body, ParameterExpression[] ps) => Expression.Lambda<Continuation.OneArgDelegate> (body, ps);
    internal static Expression DynInv(params Expression[] xs) {
        return Expression.Dynamic(
            binder: new MyInvokeBinder(new CallInfo(xs.Length -1)),
            //TODO: will dynamic invoke always be an Action? should returnType by void?
            returnType: typeof(object),
            arguments: xs);
    }


    private class LambdaExprET : ET {

        public LambdaExprET(LexicalContext scope, List.NonEmpty args) {
            // args will be something like ((a b) body..) but may or may not be syntax objects
            Expr lambdaParameters = args.Car is SyntaxObject stx ? SyntaxObject.ToDatum(stx) : args.Car; // TODO: do we want to throw this info out already?
            List.NonEmpty lambdaBody = args.Cdr as List.NonEmpty ?? throw new Exception($"malformed lambda: no body");

            var k = Expression.Parameter(typeof(Delegate), "k in MakeProcET"); // this is the continuation paramter for the proc we are making
            LexicalContext lambdaScope;
            ConstructorInfo constructor = typeof(Procedure).GetConstructor(new Type[] {typeof(Delegate)}) ?? throw new Exception("could not find constructor for Procedure");
            switch (lambdaParameters) {
                case List properList:
                    IEnumerable<Expr.Symbol> properListSymbols = properList.Cast<Expr.Symbol>();
                    if (properListSymbols is null) throw new Exception($"malformed lambda: expected parameters to be symbols but got {properList}");
                    lambdaScope = scope.Extend(properListSymbols);
                    LambdaExpressionBody = LambdaBody(k, lambdaScope, lambdaBody);
                    LambdaExpressionParams = new ParameterExpression[] {k}.Concat(lambdaScope.Parameters).ToArray();
                    Body = DynInv(kParam, // here kParam is the continuation when the lambda expression is being evaluated
                                             Expression.New(constructor,
                                                            new Expression[] {
                                                                Expression.Lambda(body: LambdaExpressionBody,
                                                                                  parameters: LambdaExpressionParams)
                                                            }));
                    return;
                case Expr.Symbol onlyRestSymbol: // cases like (lambda x x)
                    lambdaScope = scope.Extend(new Expr.Symbol[]{onlyRestSymbol});
                    LambdaExpressionBody = LambdaBody(k, lambdaScope, lambdaBody);
                    LambdaExpressionParams = new ParameterExpression[] {k}.Concat(lambdaScope.Parameters).ToArray();

                    Body = DynInv(kParam, // here kParam is the continuation when the lambda expression is being evaluated
                                             Expression.New(constructor,
                                                            new Expression[] {
                                                                Expression.Lambda(delegateType: typeof(ListFunction),
                                                               body: LambdaExpressionBody,
                                                               parameters: LambdaExpressionParams)
                                                            }));
                    return;
                case IPair andRest: // cases like (lambda (proc l . ls))
                    IEnumerable<Expr.Symbol> symbols = ValidateAndConvertToSymbols(andRest);
                    lambdaScope = scope.Extend(symbols);
                    LambdaExpressionBody = LambdaBody(k, lambdaScope, lambdaBody);
                    LambdaExpressionParams = new ParameterExpression[] {k}.Concat(lambdaScope.Parameters).ToArray();
                    Body = DynInv(kParam, // here kParam is the continuation when the lambda expression is being evaluated
                                             Expression.New(constructor,
                                                            new Expression[] {
                                                                Expression.Lambda(delegateType: FunctionTypeFromParameters(andRest),
                                                               body: LambdaExpressionBody,
                                                               parameters: LambdaExpressionParams)
                                                            }));
                    return;

                default:
                    throw new Exception($"in MakeProcET: can't handle parameters with type {lambdaParameters.GetType()} yet.");
            }

        }

        public Expression LambdaExpressionBody {get;}

        public ParameterExpression[] LambdaExpressionParams {get;}

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

        private Expression LambdaBody(ParameterExpression k, LexicalContext scope, List.NonEmpty exprs) {
            // k is the paramter continuation for the procedure that is being made (the continuation when that proc is applied)
            var block = new BlockET(scope.Extend(), exprs);
            var cont = Expression.Parameter(typeof(Delegate)); // this is the continuation parameter for the inner lambda

            // this should represent what code to run when the procedure is called
            return Expression.Invoke(
                Expression.Lambda<Action<Delegate>>(
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

    public BlockET(LexicalContext scope, List.NonEmpty exprs)
    {
        MakeListDelegate listProc = List.NewListFromObjects;
        LexicalContext blockScope = scope.Extend();
        IEnumerable<Expression<CompiledCode>> analyzed = exprs.Select(x => (Expression<CompiledCode>)Analyze(blockScope, x).Reduce());
        var listExpr = Expression.Convert(Expression.Invoke(Expression.Constant(listProc), Expression.NewArrayInit(typeof(CompiledCode), analyzed)), typeof(List.NonEmpty));

        Body = Expression.Block(blockScope.Parameters,
            new Expression[] {Expression.Invoke(
            Expression.Constant((Action<Delegate, IEnvironment, List.NonEmpty>) doSequence),
            new Expression[] {
                kParam,
                envParam,
                listExpr
            }
                )});
    }

    private void doSequence(Delegate k, IEnvironment env, List list) {
        // TODO: make caller guarantee properlist
        List.NonEmpty? exprs = list as List.NonEmpty;
        if (exprs == null) throw new Exception("doSequence: should not be called with empty list");
        var code = exprs.Car as LiteralExpr<CompiledCode> ?? throw new Exception($"BlockET.doSequence: expected a CompiledCode but got {exprs.Car.GetType()}");
        List cdr = (List)exprs.Cdr;
        Continuation.OneArgDelegate k0 = (v) => doSequence(k,env,(List.NonEmpty)cdr);
        Delegate cont = cdr is Expr.NullType ? k : k0;
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
