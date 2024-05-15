using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Dynamic;
using System.Linq.Expressions;
using System.Reflection;
using Microsoft.Scripting.Actions;

namespace Jig;

internal delegate Thunk ListFunction(Delegate k, List rest);
internal delegate Thunk PairFunction(Delegate k, Expr arg0, List rest);
internal delegate Thunk ImproperListFunction2(Delegate k, Expr arg0, Expr arg1, List rest);
internal delegate Thunk ImproperListFunction3(Delegate k, Expr arg0, Expr arg1, Expr arg2, List rest);
internal delegate Thunk ImproperListFunction4(Delegate k, Expr arg0, Expr arg1, Expr arg2, Expr arg3, List rest);
internal delegate Thunk ImproperListFunction5(Delegate k, Expr arg0, Expr arg1, Expr arg2, Expr arg3, Expr arg4, List rest);
internal delegate Thunk ImproperListFunction6(Delegate k, Expr arg0, Expr arg1, Expr arg2, Expr arg3, Expr arg4, Expr arg5, List rest);
internal delegate Thunk ImproperListFunction7(Delegate k, Expr arg0, Expr arg1, Expr arg2, Expr arg3, Expr arg4, Expr arg5, Expr arg6, List rest);

internal abstract class ET : Expression {

    public CompiledCode Compile() {
        return Expression.Lambda<CompiledCode>(Body, new ParameterExpression[] {kParam, envParam}).Compile();
    }

    protected ET() {
        kParam = Expression.Parameter(typeof(Delegate));
        envParam = Expression.Parameter(typeof(IEnvironment));
    }

    public static ET Analyze(LexicalContext scope, ParsedExpr x) =>
        x switch {
        ParsedLambda lambdaExpr => new LambdaExprET(scope, lambdaExpr),
        ParsedIf ifExpr => new IfET(scope, ifExpr),
        ParsedDefine defineExpr => new DefineET(scope, defineExpr),
        ParsedSet setExpr => new SetBangET(scope, setExpr),
        ParsedLiteral litExpr => new LiteralET(litExpr),
        ParsedQuoteSyntax quoteSyntax => new SyntaxLiteralET(quoteSyntax),
        ParsedVariable variable => new SymbolET(scope, variable),
        ParsedList list => new ProcAppET(scope, list),
        _ => throw new NotImplementedException()

    };

    public static ET Analyze(LexicalContext scope, Expr ast) {
        if (ast is ParsedIf ifExpr) {
            return new IfET(scope, ifExpr);
        }
        if (ast is ParsedLambda lambdaExpr) {
            return new LambdaExprET(scope, lambdaExpr);
        }
        if (ast is ParsedDefine defineExpr) {
            return new DefineET(scope, defineExpr);
        }
        if (ast is ParsedSet setExpr) {
            return new SetBangET(scope, setExpr);
        }
        if (ast is ParsedLiteral litExpr) {
            return new LiteralET(litExpr);
        }
        if (ast is ParsedQuoteSyntax quoteSyntaxExpr) {
            return new SyntaxLiteralET(quoteSyntaxExpr);
        }
        if (ast is ParsedVariable parsedVariable) {
            return new SymbolET(scope, parsedVariable);
        }
        if (ast is ParsedList parsedList) {
            return new ProcAppET(scope, parsedList);
        }
        if (Expr.IsLiteral(ast) || ast is Expr.VoidType) {
            return new LiteralET(ast);
        } else if (Expr.IsSymbol(ast)) {
            return new SymbolET(scope, ast);
        } else if (Expr.IsNonEmptyList(ast)) {
            List.NonEmpty list = ast is Syntax stx ? (List.NonEmpty)Syntax.E(stx) : (List.NonEmpty)ast;
            if (Expr.IsKeyword("quote", ast)) {
                // TODO: QuoteET
                return new LiteralET(list.ElementAt(1));
            } else if (Expr.IsKeyword("quote-syntax", ast)) {
                return new SyntaxLiteralET(ast);
            } else if (Expr.IsKeyword("if", ast)) {
                return new IfET(scope, list);
            } else if (Expr.IsKeyword("lambda", ast)) {
                return new LambdaExprET(scope, list);
            } else if (Expr.IsKeyword("define", ast)) {
                return new DefineET(scope, list);
            } else if (Expr.IsKeyword("set!", ast)) {
                return new SetBangET(scope, list);
            } else {
                return new ProcAppET(scope, list);
            }
        } else {
            SrcLoc? srcLoc = ast is Syntax stx ? stx.SrcLoc : null;
            throw new Exception($"Analyze: doesn't know what to do with {ast}" + (srcLoc is not null ? $" @ {srcLoc}" : ""));
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
            x = x is Syntax stx ? Syntax.ToDatum(stx) : x;
            Body = Expression.Convert(DynInv(kParam, Expression.Constant(x)), typeof(Thunk));
        }

        public LiteralET(ParsedLiteral lit) : base() {
            Expr x = Syntax.ToDatum(lit.Quoted);
            Body = Expression.Convert(DynInv(kParam, Expression.Constant(x)), typeof(Thunk));
        }

        public override Expression Body {get;}

    }

    private class SyntaxLiteralET : ET {

        public SyntaxLiteralET(ParsedQuoteSyntax parsedQuoteSyntax) {
            Body = Expression.Convert(DynInv(kParam, Expression.Constant(parsedQuoteSyntax.Quoted)), typeof(Thunk));
        }

        public SyntaxLiteralET(Expr x) : base() {
            if (x is Syntax stx) {
                if (Syntax.ToList(stx, out List? syntaxList)) {
                    Body = Expression.Convert(DynInv(kParam, Expression.Constant(syntaxList.ElementAt<Expr>(1))), typeof(Thunk));
                } else {
                    throw new Exception($"malformed syntax {stx}");
                }
            } else {
                // expression was made with read rather than read-syntax
                Body = Expression.Convert(DynInv(kParam, Expression.Constant(x)), typeof(Thunk));

            }
        }

        public override Expression Body {get;}

    }

    private class SymbolET : ET {

        private static MethodInfo LookUp {get;} = typeof(IEnvironment).GetMethod("LookUp") ?? throw new Exception("in SymbolET: IEnvironment really should have a 'LookUp' method");

        public SymbolET(LexicalContext scope, ParsedVariable variable) {
            if (variable is ParsedVariable.TopLevel topLevel) {
                var v = Expression.Parameter(typeof(Expr));
                var k = Expression.Lambda<Continuation.OneArgDelegate>(Expression.Convert(DynInv(kParam, v), typeof(Thunk)), new ParameterExpression[] {v});
                Body = Expression.Call(envParam,
                                       LookUp,
                                       new Expression [] {k, Expression.Constant(variable.Identifier)});
            } else {
                ParameterExpression? pe = scope.LookUp(variable.Identifier);
                if (pe is null) {
                    var v = Expression.Parameter(typeof(Expr));
                    var k = Expression.Lambda<Continuation.OneArgDelegate>(Expression.Convert(DynInv(kParam, v), typeof(Thunk)), new ParameterExpression[] {v});
                    Body = Expression.Call(envParam,
                                        LookUp,
                                        new Expression [] {k, Expression.Constant(variable.Identifier)});
                } else {
                    Body = Expression.Convert(DynInv(kParam, pe), typeof(Thunk));
                }
            }
        }

        public SymbolET (LexicalContext scope, Expr x) {
            ParameterExpression? pe = scope.LookUp(x);
            if (pe is null) {
                var v = Expression.Parameter(typeof(Expr));
                var k = Expression.Lambda<Continuation.OneArgDelegate>(Expression.Convert(DynInv(kParam, v), typeof(Thunk)), new ParameterExpression[] {v});
                Body = Expression.Call(envParam,
                                       LookUp,
                                       new Expression [] {k, Expression.Constant(x)});
            } else {
                Body = Expression.Convert(DynInv(kParam, pe), typeof(Thunk));
            }
        }

        public override Expression Body {get;}
    }

    internal delegate List MakeListDelegate(params CompiledCode[] args);

    delegate Thunk MapInternalDelegate(Continuation.OneArgDelegate continuation, Func<Continuation.OneArgDelegate, LiteralExpr<CompiledCode>, Thunk> proc, List list );

    delegate Thunk ApplyDelegate(Delegate k, Procedure proc, List args);
    static PropertyInfo carPropertyInfo = typeof(IPair).GetProperty("Car") ?? throw new Exception("in ProcAppET: ProperLists should have one property named 'Car'");
    static PropertyInfo cdrPropertyInfo = typeof(IPair).GetProperty("Cdr") ?? throw new Exception("in ProcAppET: ProperLists should have one property named 'Cdr'");


    private class ProcAppET : ET {

        public ProcAppET(LexicalContext scope, ParsedList list) : base () {
            IEnumerable<Expression<CompiledCode>> analyzed =
                list.ParsedExprs.Select(x => (Expression<CompiledCode>)Analyze(scope, x).Reduce());
            var vParam = Expression.Parameter(typeof(Expr));
            var v = Expression.Parameter(typeof(Expr));
            var contParam = Expression.Parameter(typeof(Continuation.OneArgDelegate));
            var procParam = Expression.Parameter(typeof(LiteralExpr<CompiledCode>));
            MakeListDelegate listProc = List.NewListFromObjects;
            var k = Expression.Lambda<Continuation.OneArgDelegate>(
                            Expression.Convert(DynInv(Expression.Constant((ApplyDelegate)Builtins.apply),
                                   kParam,
                                   Expression.Property(Expression.Convert(vParam, typeof(IPair)), carPropertyInfo),
                                   Expression.Property(Expression.Convert(vParam, typeof(IPair)), cdrPropertyInfo)),
                                               typeof(Thunk)),
                           parameters: new ParameterExpression[] {vParam});
            Body =
                Expression.Convert(
                    DynInv(Expression.Constant((MapInternalDelegate) Builtins.map_internal),
                       k,
                       Expression.Lambda<Func<Continuation.OneArgDelegate, LiteralExpr<CompiledCode>, Thunk>>( // (lambda (k code) (code k env))
                           body: Expression.Convert(DynInv(Expression.Property(procParam, "Value"), contParam, envParam), typeof(Thunk)),
                           parameters: new ParameterExpression[] {contParam, procParam}),
                       Expression.Invoke(Expression.Constant(listProc), Expression.NewArrayInit(typeof(CompiledCode), analyzed))),
                    typeof(Thunk));

        }

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
                            Expression.Convert(DynInv(Expression.Constant((ApplyDelegate)Builtins.apply),
                                   kParam,
                                   Expression.Property(Expression.Convert(vParam, typeof(IPair)), carPropertyInfo),
                                   Expression.Property(Expression.Convert(vParam, typeof(IPair)), cdrPropertyInfo)),
                                               typeof(Thunk)),
                           parameters: new ParameterExpression[] {vParam});
            Body =
                Expression.Convert(
                    DynInv(Expression.Constant((MapInternalDelegate) Builtins.map_internal),
                       k,
                       Expression.Lambda<Func<Continuation.OneArgDelegate, LiteralExpr<CompiledCode>, Thunk>>( // (lambda (k code) (code k env))
                           body: Expression.Convert(DynInv(Expression.Property(procParam, "Value"), contParam, envParam), typeof(Thunk)),
                           parameters: new ParameterExpression[] {contParam, procParam}),
                       Expression.Invoke(Expression.Constant(listProc), Expression.NewArrayInit(typeof(CompiledCode), analyzed))),
                    typeof(Thunk));
        }

        public override Expression Body {get;}
    }

    private class IfET : ET {

        public IfET(LexicalContext lexVars, ParsedIf ifExpr) {

            Expr cond = ifExpr.Condition;
            Expression<CompiledCode> condCC = (Expression<CompiledCode>)Analyze(lexVars, cond).Reduce();
            Expr consq = ifExpr.Then;
            Expression<CompiledCode> consqCC = (Expression<CompiledCode>)Analyze(lexVars, consq).Reduce();
            Expr alt;
            Expression ifFalseExpr;
            if (ifExpr.Else is not null) {
                alt = ifExpr.Else;
            } else {
                alt = Expr.Void;
            }
            ifFalseExpr = Expression.Lambda<Thunk>(
                    Expression.Convert(DynInv((Expression<CompiledCode>)Analyze(lexVars, alt).Reduce(), kParam, envParam),
                                       typeof(Thunk)));
            ParameterExpression boolResult = Expression.Parameter(typeof(Expr), "boolResult");
            var k0 = Expression.Lambda<Continuation.OneArgDelegate>(
                body: Expression.Condition( // the Condition Expression returns something. IfThenElse is void
                    test: Expression.Convert(DynInv(Expression.Constant((Func<Expr, bool>) IfET.IsNotFalse), boolResult), typeof(bool)),
                    ifTrue: Expression.Lambda<Thunk>(Expression.Convert(DynInv(consqCC, kParam, envParam), typeof(Thunk))),
                    ifFalse: ifFalseExpr),
                parameters: new ParameterExpression[] {boolResult});
            Body = Expression.Invoke(condCC, new Expression[] {k0, envParam});

        }

        public IfET(LexicalContext lexVars, List.NonEmpty list) : base() {

            // TODO: In scheme, an if form can have no else expr, like this: (if #t 1)
            List.NonEmpty listCdr = list.Cdr as List.NonEmpty ?? throw new Exception($"malformed if: {list}"); // TODO: should the parser be doing all this checking for malformed whatevers?
            Expr cond = listCdr.Car;
            Expression<CompiledCode> condCC = (Expression<CompiledCode>)Analyze(lexVars, cond).Reduce();
            List.NonEmpty listCdrCdr = listCdr.Cdr as List.NonEmpty ?? throw new Exception($"malformed if: {list}");
            Expr consq = listCdrCdr.Car;
            Expression<CompiledCode> consqCC = (Expression<CompiledCode>)Analyze(lexVars, consq).Reduce();
            // TODO: actually, if doesn't need an else branch
            List.NonEmpty listCdrCdrCdr;
            Expr alt;
            Expression ifFalseExpr;
            ParameterExpression boolResult = Expression.Parameter(typeof(Expr), "boolResult");
            if (list.Count() == 4) {

                listCdrCdrCdr = listCdrCdr.Cdr as List.NonEmpty ?? throw new Exception($"malformed if: {list}");
                alt = listCdrCdrCdr.Car;
            } else if (list.Count() == 3) {
                alt = Expr.Void;
            } else {
                throw new Exception($"malformed if: {list}");

            }
            ifFalseExpr = Expression.Lambda<Thunk>(
                    Expression.Convert(DynInv((Expression<CompiledCode>)Analyze(lexVars, alt).Reduce(), kParam, envParam),
                                       typeof(Thunk)));
            var k0 = Expression.Lambda<Continuation.OneArgDelegate>(
                body: Expression.Condition( // the Condition Expression returns something. IfThenElse is void
                    test: Expression.Convert(DynInv(Expression.Constant((Func<Expr, bool>) IfET.IsNotFalse), boolResult), typeof(bool)),
                    ifTrue: Expression.Lambda<Thunk>(Expression.Convert(DynInv(consqCC, kParam, envParam), typeof(Thunk))),
                    ifFalse: ifFalseExpr),
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

        public SetBangET(LexicalContext lexVars, ParsedSet setExpr) : base() {
            Syntax.Identifier sym = setExpr.Variable.Identifier;
            // if (sym.Symbol.Name == "z") {
            //     Console.WriteLine("set!");
            // }
            Syntax valExpr = setExpr.Value;
            Expression<CompiledCode> valCC = (Expression<CompiledCode>)Analyze(lexVars, valExpr).Reduce();
            ParameterExpression val = Expression.Parameter(typeof(Expr), "val");
            Expression contBody;
            if (setExpr.Variable is ParsedVariable.TopLevel) {
                contBody = Expression.Call(envParam,
                                       _setMethod,
                                       new Expression [] {kParam, Expression.Constant(sym), val});
            } else {
                ParameterExpression? pe = lexVars.LookUp(sym);
                if (pe is null) {
                    contBody = Expression.Call(envParam,
                                        _setMethod,
                                        new Expression [] {kParam, Expression.Constant(sym), val});
                } else {
                    contBody = DynInv(kParam, Expression.Block(Expression.Assign(pe, val), Expression.Constant(Expr.Void)));
                }
            }
            var k = Expression.Lambda(contBody, new ParameterExpression [] {val});

            Body = Expression.Invoke(valCC, new Expression[] {k, envParam});
        }

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
                contBody = DynInv(kParam, Expression.Block(Expression.Assign(pe, val), Expression.Constant(Expr.Void)));
            }
            var k = Expression.Lambda(contBody, new ParameterExpression [] {val});

            Body = Expression.Invoke(valCC, new Expression[] {k, envParam});
        }

        public override Expression Body {get;}
    }

    private class DefineET : ET {

        private static MethodInfo _defineMethod {get;} = typeof(IEnvironment).GetMethod("Define") ?? throw new Exception("while initializing DefineET, could not find 'Define' method on IEnvironment");

        public DefineET(LexicalContext lexVars, ParsedDefine defineExpr) : base() {
            // TODO: can't have recursive definitions inside blocks ,eg:
            // (begin (define loop (lambda (n) (if (= n 0) n (loop (- n 1))))) (loop 3))
            Syntax.Identifier sym = defineExpr.Variable.Identifier;
            Syntax valExpr = defineExpr.Value;
            // if (sym.Symbol.Name == "z") {
            //     Console.WriteLine("define");
            // }
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

            // if (defineExpr.Variable is ParsedVariable.TopLevel) {
            //     contBody = Expression.Call(envParam,
            //                            _defineMethod,
            //                            new Expression [] {kParam, Expression.Constant(sym), val});
            // } else {
            //     ParameterExpression pe = lexVars.ParameterForDefine(sym);
            //     if (pe is null) {
            //         // TODO: figure out why some top levels aren't toplevels
            //         contBody = Expression.Call(envParam,
            //                             _defineMethod,
            //                             new Expression [] {kParam, Expression.Constant(sym), val});
            //     } else {
            //         contBody = DynInv(kParam, Expression.Block(Expression.Assign(pe, val), Expression.Constant(Expr.Void)));
            //     }
            // }
            Expression<CompiledCode> valCC = (Expression<CompiledCode>)Analyze(lexVars, valExpr).Reduce();


            var k = Expression.Lambda(contBody, new ParameterExpression [] {val});

            Body = Expression.Invoke(valCC, new Expression[] {k, envParam});

        }

        public DefineET(LexicalContext lexVars, List.NonEmpty list) : base() {
            // TODO: can't have recursive definitions inside blocks ,eg:
            // (begin (define loop (lambda (n) (if (= n 0) n (loop (- n 1))))) (loop 3))
            Expr sym = list.ElementAt(1);
            Expr valExpr = list.ElementAt(2);
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
            Expression<CompiledCode> valCC = (Expression<CompiledCode>)Analyze(lexVars, valExpr).Reduce();


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
        static ConstructorInfo procedureCstr = typeof(Procedure).GetConstructor(new Type[] {typeof(Delegate)}) ?? throw new Exception("could not find constructor for Procedure");

        public LambdaExprET(LexicalContext scope, ParsedLambda lambdaExpr) {
            Syntax lambdaParameters = lambdaExpr.Parameters;
            SyntaxList lambdaBody = lambdaExpr.Bodies;

            var k = Expression.Parameter(typeof(Delegate), "k in LambdaExprET"); // this is the continuation paramter for the proc we are making
            LexicalContext lambdaScope;
            Expr lambdaParameters_E = Syntax.E(lambdaParameters);
            switch (lambdaParameters_E) {
                case List properList:
                    IEnumerable<Syntax.Identifier> properListSymbols = properList.Cast<Syntax.Identifier>();
                    if (properListSymbols is null) throw new Exception($"malformed lambda: expected parameters to be symbols but got {properList}");
                    lambdaScope = scope.Extend(properListSymbols.Select(i => i.Symbol));
                    LambdaExpressionBody = LambdaBody(k, lambdaScope, lambdaBody);
                    LambdaExpressionParams = new ParameterExpression[] {k}.Concat(lambdaScope.Parameters).ToArray();
                    Body =
                        Expression.Convert(
                            DynInv(kParam,
                                   Expression.New(
                                       procedureCstr,
                                       new Expression[] {
                                           Expression.Lambda(body: LambdaExpressionBody,
                                                             parameters: LambdaExpressionParams)
                                       })),
                            typeof(Thunk));
                    return;
                case Expr.Symbol onlyRestSymbol: // cases like (lambda x x)
                    lambdaScope = scope.Extend(new Expr.Symbol[]{onlyRestSymbol});
                    LambdaExpressionBody = LambdaBody(k, lambdaScope, lambdaBody);
                    LambdaExpressionParams = new ParameterExpression[] {k}.Concat(lambdaScope.Parameters).ToArray();

                    Body = Expression.Convert(DynInv(kParam, // here kParam is the continuation when the lambda expression is being evaluated
                                             Expression.New(procedureCstr,
                                                            new Expression[] {
                                                                Expression.Lambda(delegateType: typeof(ListFunction),
                                                               body: LambdaExpressionBody,
                                                               parameters: LambdaExpressionParams)
                                                            })), typeof(Thunk));
                    return;
                case IPair andRest: // cases like (lambda (proc l . ls))
                    IEnumerable<Expr.Symbol> symbols = ValidateAndConvertToSymbols(andRest);
                    lambdaScope = scope.Extend(symbols);
                    LambdaExpressionBody = LambdaBody(k, lambdaScope, lambdaBody);
                    LambdaExpressionParams = new ParameterExpression[] {k}.Concat(lambdaScope.Parameters).ToArray();
                    Body = Expression.Convert(DynInv(kParam, // here kParam is the continuation when the lambda expression is being evaluated
                                             Expression.New(procedureCstr,
                                                            new Expression[] {
                                                                Expression.Lambda(delegateType: FunctionTypeFromParameters(andRest),
                                                               body: LambdaExpressionBody,
                                                               parameters: LambdaExpressionParams)
                                                            })), typeof(Thunk));
                    return;

                default:
                    throw new Exception($"in LambdaExprET: can't handle parameters with type {lambdaParameters.GetType()} yet. (Got {lambdaParameters_E})");
            }

        }

        public LambdaExprET(LexicalContext scope, List.NonEmpty args) {
            Expr lambdaParameters = args.ElementAt(1) is Syntax stx ? Syntax.ToDatum(stx) : args.ElementAt(1); // TODO: do we want to throw this info out already?
            Debug.Assert(args.Count() >= 3);
            List.NonEmpty lambdaBody = (List.NonEmpty)List.ListFromEnumerable(args.Skip(2));

            var k = Expression.Parameter(typeof(Delegate), "k in MakeProcET"); // this is the continuation paramter for the proc we are making
            LexicalContext lambdaScope;
            switch (lambdaParameters) {
                case List properList:
                    IEnumerable<Expr.Symbol> properListSymbols = properList.Cast<Expr.Symbol>();
                    if (properListSymbols is null) throw new Exception($"malformed lambda: expected parameters to be symbols but got {properList}");
                    lambdaScope = scope.Extend(properListSymbols);
                    LambdaExpressionBody = LambdaBody(k, lambdaScope, lambdaBody);
                    LambdaExpressionParams = new ParameterExpression[] {k}.Concat(lambdaScope.Parameters).ToArray();
                    Body =
                        Expression.Convert(
                            DynInv(kParam,
                                   Expression.New(
                                       procedureCstr,
                                       new Expression[] {
                                           Expression.Lambda(body: LambdaExpressionBody,
                                                             parameters: LambdaExpressionParams)
                                       })),
                            typeof(Thunk));
                    return;
                case Expr.Symbol onlyRestSymbol: // cases like (lambda x x)
                    lambdaScope = scope.Extend(new Expr.Symbol[]{onlyRestSymbol});
                    LambdaExpressionBody = LambdaBody(k, lambdaScope, lambdaBody);
                    LambdaExpressionParams = new ParameterExpression[] {k}.Concat(lambdaScope.Parameters).ToArray();

                    Body = Expression.Convert(DynInv(kParam, // here kParam is the continuation when the lambda expression is being evaluated
                                             Expression.New(procedureCstr,
                                                            new Expression[] {
                                                                Expression.Lambda(delegateType: typeof(ListFunction),
                                                               body: LambdaExpressionBody,
                                                               parameters: LambdaExpressionParams)
                                                            })), typeof(Thunk));
                    return;
                case IPair andRest: // cases like (lambda (proc l . ls))
                    IEnumerable<Expr.Symbol> symbols = ValidateAndConvertToSymbols(andRest);
                    lambdaScope = scope.Extend(symbols);
                    LambdaExpressionBody = LambdaBody(k, lambdaScope, lambdaBody);
                    LambdaExpressionParams = new ParameterExpression[] {k}.Concat(lambdaScope.Parameters).ToArray();
                    Body = Expression.Convert(DynInv(kParam, // here kParam is the continuation when the lambda expression is being evaluated
                                             Expression.New(procedureCstr,
                                                            new Expression[] {
                                                                Expression.Lambda(delegateType: FunctionTypeFromParameters(andRest),
                                                               body: LambdaExpressionBody,
                                                               parameters: LambdaExpressionParams)
                                                            })), typeof(Thunk));
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
            Expr car = parameters.Car;
            Expr.Symbol sym = car is Syntax.Identifier id ? id.Symbol : car is Expr.Symbol s ? s : throw new Exception($"lambda: all parameters must be symbols (given {car}");
            result.Add(sym);
            Expr cdr = parameters.Cdr;
            while (cdr is IPair pairCdr) {
                car = pairCdr.Car;
                sym = car is Syntax.Identifier i ? i.Symbol : car is Expr.Symbol sm ? sm : throw new Exception($"lambda: all parameters must be symbols (given {car}");
                result.Add(sym);
                cdr = pairCdr.Cdr;
            }
            sym = cdr is Syntax.Identifier idf ? idf.Symbol : cdr is Expr.Symbol symbol ? symbol : throw new Exception($"lambda: all paramters must be symbols (given {cdr})");
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
            // TODO: why is this so much more complicated than just returning a block expr?

            // k is the paramter continuation for the procedure that is being made (the continuation when that proc is applied)
            var block = new BlockET(scope.Extend(), exprs);
            var cont = Expression.Parameter(typeof(Delegate)); // this is the continuation parameter for the inner lambda

            // this should represent what code to run when the procedure is called
            return Expression.Invoke(
                Expression.Lambda<Func<Delegate, Thunk>>(
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

    internal class BlockET : ET {

        public BlockET(LexicalContext scope, List.NonEmpty exprs)
        {
            MakeListDelegate listProc = List.NewListFromObjects;
            LexicalContext blockScope = scope.Extend();
            // analyze and reduce all but last expr
            Expression<CompiledCode>[] analyzed = exprs
                .Take(exprs.Count() - 1)
                .Select(x => (Expression<CompiledCode>)Analyze(blockScope, x).Reduce()).ToArray();
            // forcing to array to deal with weird bug where things were done out of order when handled as IEnumerable
            // the last expr in the list has to return a MaybeThunk for trampoline
            Expression<CompiledCode> lastExprCC = (Expression<CompiledCode>)Analyze(blockScope, exprs.Last()).Reduce();
            Expression thunkExpr = Expression.Lambda<Thunk>(Expression.Convert(DynInv(lastExprCC, kParam, envParam), typeof(Thunk)));

                // NewMaybeThunkExpression(DynInv(lastExprCC, kParam, envParam));
            // have to 'Reduce' by hand because newMaybeThunk is not an ET
            Expression<CompiledCode> last = Expression.Lambda<CompiledCode>(thunkExpr, new ParameterExpression[] {kParam, envParam});
            analyzed = analyzed.ToList().Append(last).ToArray();
            var listExpr = Expression.Convert(Expression.Invoke(Expression.Constant(listProc), Expression.NewArrayInit(typeof(CompiledCode), analyzed)), typeof(List.NonEmpty));
            Body = Expression.Block(blockScope.Parameters,
                new Expression[] {Expression.Invoke(
                Expression.Constant((Func<Delegate, IEnvironment, List.NonEmpty, Thunk>) doSequence),
                new Expression[] {
                    kParam,
                    envParam,
                    listExpr
                }
                    )});
        }

        private Thunk doSequence(Delegate k, IEnvironment env, List list) {
            // TODO: make caller guarantee properlist
            List.NonEmpty? exprs = list as List.NonEmpty;
            if (exprs == null) throw new Exception("doSequence: should not be called with empty list");
            var code = exprs.Car as LiteralExpr<CompiledCode> ?? throw new Exception($"BlockET.doSequence: expected a CompiledCode but got {exprs.Car.GetType()}");
            List cdr = (List)exprs.Cdr;
            Continuation.OneArgDelegate k0 = (v) => doSequence(k,env,(List.NonEmpty)cdr);
            Delegate cont = cdr is Expr.NullType ? k : k0;
            return code.Value(cont, env);
        }

        public override Expression Body {get;}
    }

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
