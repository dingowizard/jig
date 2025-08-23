using System.Diagnostics;
using System.Dynamic;
using System.Linq.Expressions;
using System.Reflection;
using Microsoft.Scripting.Actions;

namespace Jig;

internal delegate Thunk? ListFunction(Delegate k, List rest);
internal delegate Thunk? PairFunction(Delegate k, IForm arg0, List rest);
internal delegate Thunk? ImproperListFunction2(Delegate k, IForm arg0, IForm arg1, List rest);
internal delegate Thunk? ImproperListFunction3(Delegate k, IForm arg0, IForm arg1, IForm arg2, List rest);
internal delegate Thunk? ImproperListFunction4(Delegate k, IForm arg0, IForm arg1, IForm arg2, IForm arg3, List rest);
internal delegate Thunk? ImproperListFunction5(Delegate k, IForm arg0, IForm arg1, IForm arg2, IForm arg3, IForm arg4, List rest);
internal delegate Thunk? ImproperListFunction6(Delegate k, IForm arg0, IForm arg1, IForm arg2, IForm arg3, IForm arg4, IForm arg5, List rest);
internal delegate Thunk? ImproperListFunction7(Delegate k, IForm arg0, IForm arg1, IForm arg2, IForm arg3, IForm arg4, IForm arg5, IForm arg6, List rest);

internal abstract class ET : Expression {

    public CompiledCode Compile() {
        return Lambda<CompiledCode>(Body, kParam, envParam).Compile();
    }

    protected ET() {
        kParam = Expression.Parameter(typeof(Delegate));
        envParam = Expression.Parameter(typeof(IEnvironment));
    }

    public static ET Analyze(LexicalContext scope, ParsedForm x) =>
        x switch {
        ParsedBegin parsedBegin => new BeginET(scope, parsedBegin),
        ParsedLambda lambdaExpr => new LambdaExprET(scope, lambdaExpr),
        ParsedIf ifExpr => new IfET(scope, ifExpr),
        ParsedDefine defineExpr => new DefineET(scope, defineExpr),
        ParsedSet setExpr => new SetBangET(scope, setExpr),
        ParsedLiteral litExpr => new LiteralET(litExpr),
        ParsedQuoteSyntax quoteSyntax => new SyntaxLiteralET(quoteSyntax),
        ParsedVariable variable => new SymbolET(scope, variable),
        ParsedApplication list => new ProcAppET(scope, list),
        _ => throw new NotImplementedException()

    };

    public static ET Analyze(LexicalContext scope, IForm ast) {
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
        if (ast is ParsedApplication parsedList) {
            return new ProcAppET(scope, parsedList);
        }
        if (ast is LiteralExpr || ast is Syntax.Literal) {
            return new LiteralET(ast);
        }
        if (Form.IsSymbol(ast)) {
            return new SymbolET(scope, ast);
        } else if (Form.IsNonEmptyList(ast)) {
            List.NonEmpty list = ast is Syntax stx ? (List.NonEmpty)Syntax.E(stx) : (List.NonEmpty)ast;
            if (Form.IsKeyword("quote", ast)) {
                // TODO: QuoteET
                return new LiteralET(list.ElementAt(1));
            }

            if (Form.IsKeyword("quote-syntax", ast)) {
                return new SyntaxLiteralET(ast);
            } else if (Form.IsKeyword("if", ast)) {
                return new IfET(scope, list);
            } else if (Form.IsKeyword("lambda", ast)) {
                return new LambdaExprET(scope, list);
            } else if (Form.IsKeyword("define", ast)) {
                return new DefineET(scope, list);
            } else if (Form.IsKeyword("set!", ast)) {
                return new SetBangET(scope, list);
            } else {
                return new ProcAppET(scope, list);
            }
        } else {
            SrcLoc? srcLoc = ast is Syntax stx ? stx.SrcLoc : null;
            throw new Exception($"Analyze: doesn't know what to do with {ast}, a {ast.GetType()}. Equal to LiteralExpr? {ast is LiteralExpr}" + (srcLoc is not null ? $" @ {srcLoc}" : ""));
        }
    }

    public override bool CanReduce {get;} = true;

    public override Expression Reduce() {
        return Expression.Lambda<CompiledCode>(Body, new[] {kParam, envParam});
    }

    public override ExpressionType NodeType {get;} = ExpressionType.Extension;

    public override Type Type {get;} = typeof(CompiledCode);

    private readonly ParameterExpression kParam;
    private readonly ParameterExpression envParam;
    protected abstract Expression Body {get;}

    private class LiteralET : ET {

        public LiteralET(IForm x) {
            x = x is Syntax stx ? Syntax.ToDatum(stx) : x;
            Body = Expression.Convert(DynInv(kParam, Expression.Constant(x)), typeof(Thunk));
        }

        public LiteralET(ParsedLiteral lit) {
            IForm x = Syntax.ToDatum(lit.Quoted);
            Body = Expression.Convert(DynInv(kParam, Expression.Constant(x)), typeof(Thunk));
        }

        protected override Expression Body {get;}

    }

    private class SyntaxLiteralET : ET {

        public SyntaxLiteralET(ParsedQuoteSyntax parsedQuoteSyntax) {
            Body = Expression.Convert(DynInv(kParam, Expression.Constant(parsedQuoteSyntax.Quoted)), typeof(Thunk));
        }

        public SyntaxLiteralET(IForm x) {
            if (x is Syntax stx) {
                if (Syntax.ToList(stx, out SyntaxList? syntaxList)) {
                    Body = Expression.Convert(DynInv(kParam, Expression.Constant(syntaxList.ElementAt<Form>(1))), typeof(Thunk));
                } else {
                    throw new Exception($"malformed syntax {stx}");
                }
            } else {
                // expression was made with read rather than read-syntax
                Body = Expression.Convert(DynInv(kParam, Expression.Constant(x)), typeof(Thunk));

            }
        }

        protected override Expression Body {get;}

    }

    private class SymbolET : ET {

        private static MethodInfo LookUp {get;} = typeof(IEnvironment).GetMethod("LookUp") ?? throw new Exception("in SymbolET: IEnvironment really should have a 'LookUp' method");

        public SymbolET(LexicalContext scope, ParsedVariable variable) {
            if (variable is ParsedVariable.TopLevel) {
                var v = Expression.Parameter(typeof(IForm));
                var k = Lambda<Continuation.OneArgDelegate>(Expression.Convert(DynInv(kParam, v), typeof(Thunk)), v);
                Body = Expression.Call(envParam,
                                       LookUp,
                                       [k, Constant(variable.Identifier)]);
            } else {
                ParameterExpression? pe = scope.LookUp(variable.Identifier);
                if (pe is null) {
                    throw new Exception($"free variable: {variable.Identifier.Symbol} @ {variable.Identifier.SrcLoc }");
                    // var v = Expression.Parameter(typeof(Expr));
                    // var k = Expression.Lambda<Continuation.OneArgDelegate>(Expression.Convert(DynInv(kParam, v), typeof(Thunk)), new ParameterExpression[] {v});
                    // Body = Expression.Call(envParam,
                    //                     LookUp,
                    //                     new Expression [] {k, Expression.Constant(variable.Identifier)});
                } else {
                    Body = Expression.Convert(DynInv(kParam, pe), typeof(Thunk));
                }
            }
        }

        public SymbolET (LexicalContext scope, IForm x) {
            ParameterExpression? pe = scope.LookUp(x);
            if (pe is null) {
                var v = Expression.Parameter(typeof(IForm));
                var k = Expression.Lambda<Continuation.OneArgDelegate>(Expression.Convert(DynInv(kParam, v), typeof(Thunk)), v);
                Body = Expression.Call(envParam,
                                       LookUp,
                                       [k, Expression.Constant(x)]);
            } else {
                Body = Expression.Convert(DynInv(kParam, pe), typeof(Thunk));
            }
        }

        protected override Expression Body {get;}
    }


    delegate Thunk? MapInternalDelegate(Continuation.OneArgDelegate continuation, IEnvironment env, CompiledCode[] list, int index);

    delegate Thunk? ApplyDelegate(Delegate k, IForm proc, List args);
    static readonly PropertyInfo carPropertyInfo = typeof(IPair).GetProperty("Car") ?? throw new Exception("in ProcAppET: ProperLists should have one property named 'Car'");
    static readonly PropertyInfo restPropertyInfo = typeof(List.NonEmpty).GetProperty("Rest") ?? throw new Exception("in ProcAppET: ProperLists should have one property named 'Cdr'");


    private class ProcAppET : ET {

        public ProcAppET(LexicalContext scope, ParsedApplication list) {
            // if (ListContainsY(list)) {
            //     Console.WriteLine($"ProcAppET: {list}");
            // }
            IEnumerable<Expression<CompiledCode>> analyzed =
                list.ParsedExprs.Select(x => (Expression<CompiledCode>)Analyze(scope, x).Reduce());
            var vParam = Parameter(typeof(IForm));
            var k = Lambda<Continuation.OneArgDelegate>(
                            Convert(
                                Invoke(Constant((ApplyDelegate)Builtins.apply),
                                                  kParam,
                                                  Property(Convert(vParam, typeof(IPair)), carPropertyInfo),
                                                  Property(Convert(vParam, typeof(List.NonEmpty)), restPropertyInfo)),
                                typeof(Thunk)),
                           parameters: [vParam]);
            Body =
                Convert(
                    Invoke(
                        Constant((MapInternalDelegate) Builtins.map_internal),
                        k,
                        envParam,
                        NewArrayInit(typeof(CompiledCode), analyzed),
                        Constant(0)),
                    typeof(Thunk));

        }

        public ProcAppET(LexicalContext scope, List.NonEmpty list) {
            // TODO: probably there is a more certain way of checking to see that we have syntax pair?
            IEnumerable<Expression<CompiledCode>> analyzed =
                list.Select(x => (Expression<CompiledCode>)Analyze(scope, x).Reduce());
            var vParam = Parameter(typeof(IForm));
            var k = Lambda<Continuation.OneArgDelegate>(
                            Convert(
                                DynInv(
                                    Constant((ApplyDelegate)Builtins.apply),
                                    kParam,
                                    Property(Convert(vParam, typeof(IPair)), carPropertyInfo),
                                    Property(Convert(vParam, typeof(List.NonEmpty)), restPropertyInfo)),
                                typeof(Thunk)),
                           parameters: [vParam]);
            Body =
                Convert(
                    Invoke(Constant((MapInternalDelegate) Builtins.map_internal),
                           k,
                           envParam,
                           NewArrayInit(typeof(CompiledCode), analyzed),
                           Constant(0)),
                    typeof(Thunk));
        }

        protected override Expression Body {get;}
    }

    private class IfET : ET {

        public IfET(LexicalContext lexVars, ParsedIf ifExpr) {

            ParsedForm cond = ifExpr.Condition;
            Expression<CompiledCode> condCC = (Expression<CompiledCode>)Analyze(lexVars, cond).Reduce();
            ParsedForm consq = ifExpr.Then;
            Expression<CompiledCode> consqCC = (Expression<CompiledCode>)Analyze(lexVars, consq).Reduce();
            ParsedForm alt;
            if (ifExpr.Else is not null) {
                alt = ifExpr.Else;
            } else {
                if(ParsedLiteral.TryParse(new Syntax(Form.Void), out var voidLiteral)) {
                    alt = voidLiteral;
                } else {
                    throw new Exception("omg! couldn't parse void in IfET ");
                }
            }
            Expression ifFalseExpr = Lambda<Thunk>(
                Expression.Convert(DynInv((Expression<CompiledCode>)Analyze(lexVars, alt).Reduce(), kParam, envParam),
                    typeof(Thunk)));
            ParameterExpression boolResult = Parameter(typeof(IForm), "boolResult");
            var k0 = Lambda<Continuation.OneArgDelegate>(
                body: Condition( // the Condition Expression returns something. IfThenElse is void
                    test: Convert(DynInv(Constant((Func<Form, bool>) IfET.IsNotFalse), boolResult), typeof(bool)),
                    ifTrue: Lambda<Thunk>(Convert(DynInv(consqCC, kParam, envParam), typeof(Thunk))),
                    ifFalse: ifFalseExpr),
                parameters: [boolResult]);
            Body = Invoke(condCC, k0, envParam);

        }

        public IfET(LexicalContext lexVars, List.NonEmpty list) {

            List.NonEmpty listCdr = list.Cdr as List.NonEmpty ?? throw new Exception($"malformed if: {list}"); // TODO: should the parser be doing all this checking for malformed whatevers?
            IForm cond = listCdr.Car;
            Expression<CompiledCode> condCC = (Expression<CompiledCode>)Analyze(lexVars, cond).Reduce();
            List.NonEmpty listCdrCdr = listCdr.Cdr as List.NonEmpty ?? throw new Exception($"malformed if: {list}");
            IForm consq = listCdrCdr.Car;
            Expression<CompiledCode> consqCC = (Expression<CompiledCode>)Analyze(lexVars, consq).Reduce();
            // TODO: actually, if doesn't need an else branch
            IForm alt;
            ParameterExpression boolResult = Expression.Parameter(typeof(IForm), "boolResult");
            if (list.Count() == 4) {
                var listCdrCdrCdr = listCdrCdr.Cdr as List.NonEmpty ?? throw new Exception($"malformed if: {list}");
                alt = listCdrCdrCdr.Car;
            } else if (list.Count() == 3) {
                alt = Form.Void;
            } else {
                throw new Exception($"malformed if: {list}");

            }
            Expression ifFalseExpr = Expression.Lambda<Thunk>(
                Expression.Convert(DynInv((Expression<CompiledCode>)Analyze(lexVars, alt).Reduce(), kParam, envParam),
                    typeof(Thunk)));
            var k0 = Expression.Lambda<Continuation.OneArgDelegate>(
                body: Condition( // the Condition Expression returns something. IfThenElse is void
                    test: Convert(DynInv(Constant((Func<Form, bool>) IfET.IsNotFalse), boolResult), typeof(bool)),
                    ifTrue: Lambda<Thunk>(Convert(DynInv(consqCC, kParam, envParam), typeof(Thunk))),
                    ifFalse: ifFalseExpr),
                parameters: [boolResult]);
            Body = Invoke(condCC, k0, envParam);

        }

        protected override Expression Body {get;}
        private static bool IsNotFalse(Form x)
        {
            if (x is Bool boolExpr) {
                return boolExpr.Value;
            }

            return true;
        }
    }

    private class BeginET : ET {
        public BeginET(LexicalContext scope, ParsedBegin parsedBegin) {
            var analyzed = parsedBegin.Forms
                .Select(x => (Expression<CompiledCode>)Analyze(scope, x).Reduce()).ToArray<Expression>();
            Body = Expression.Invoke(
                        Expression.Constant((Func<Delegate, IEnvironment, CompiledCode[], int, Thunk>) BlockET.doSequence),
                        kParam,
                        envParam,
                        Expression.NewArrayInit(typeof(CompiledCode), analyzed),
                        Expression.Constant(0));
        }

        protected override Expression Body {get;}
    }

    private class SetBangET : ET {

        private static MethodInfo _setMethod {get;} = typeof(IEnvironment).GetMethod("Set") ?? throw new Exception("while initializeing SetBangET, could not find 'Set' method on IEnvironment");

        public SetBangET(LexicalContext lexVars, ParsedSet setExpr) {
            Syntax.Identifier sym = setExpr.Variable.Identifier;
            // if (id.Symbol.Name == "z") {
            //     Console.WriteLine("set!");
            // }
            Syntax valExpr = setExpr.Value;
            Expression<CompiledCode> valCC = (Expression<CompiledCode>)Analyze(lexVars, valExpr).Reduce();
            ParameterExpression val = Expression.Parameter(typeof(Form), "val");
            Expression contBody;
            if (setExpr.Variable is ParsedVariable.TopLevel) {
                contBody = Expression.Call(envParam,
                                       _setMethod,
                                       [kParam, Expression.Constant(sym), val]);
            } else {
                ParameterExpression? pe = lexVars.LookUp(sym);
                if (pe is null) {
                    throw new Exception($"free-variable: {sym.Symbol} @ {(sym.SrcLoc?.ToString() ?? "" )}");
                } else {
                    contBody = DynInv(kParam, Expression.Block(Expression.Assign(pe, val), Expression.Constant(Form.Void)));
                }
            }
            var k = Expression.Lambda(contBody, [val]);

            Body = Expression.Invoke(valCC, [k, envParam]);
        }

        public SetBangET(LexicalContext lexVars, List.NonEmpty list) {
            IForm sym = list.ElementAt(1);
            IForm valExpr = list.ElementAt(2);
            Expression<CompiledCode> valCC = (Expression<CompiledCode>)Analyze(lexVars, valExpr).Reduce();
            ParameterExpression val = Expression.Parameter(typeof(Form), "val");
            Expression contBody;
            ParameterExpression? pe = lexVars.LookUp(sym);
            if (pe is null) {
                contBody = Expression.Call(envParam,
                                       _setMethod,
                                       [kParam, Expression.Constant(sym), val]);
            } else {
                contBody = DynInv(kParam, Expression.Block(Expression.Assign(pe, val), Expression.Constant(Form.Void)));
            }
            var k = Expression.Lambda(contBody, [val]);

            Body = Expression.Invoke(valCC, [k, envParam]);
        }

        protected override Expression Body {get;}
    }

    private class DefineET : ET {

        private static MethodInfo _defineMethod {get;} = typeof(IEnvironment).GetMethod("Define") ?? throw new Exception("while initializing DefineET, could not find 'Define' method on IEnvironment");

        public DefineET(LexicalContext lexVars, ParsedDefine defineExpr) {
            Syntax.Identifier id = defineExpr.Variable.Identifier;
            Syntax valExpr = defineExpr.Value;
            ParameterExpression val = Expression.Parameter(typeof(Form), "val");
            Expression contBody;
            if (lexVars.AtTopLevel()) {
                    contBody = Expression.Call(envParam,
                                               _defineMethod,
                                               [kParam, Expression.Constant(id), val]);
            } else {
                ParameterExpression pe = lexVars.ParameterForDefine(id);
                contBody = DynInv(kParam, Expression.Assign(pe, val));
            }
            Expression<CompiledCode> valCC = (Expression<CompiledCode>)Analyze(lexVars, valExpr).Reduce();
            var k = Expression.Lambda(contBody, [val]);
            Body = Expression.Invoke(valCC, [k, envParam]);

        }

        public DefineET(LexicalContext lexVars, List.NonEmpty list)
        {
            IForm sym = list.ElementAt(1);
            IForm valExpr = list.ElementAt(2);
            ParameterExpression val = Expression.Parameter(typeof(Form), "val");
            Expression contBody;
            if (lexVars.AtTopLevel()) {
                    contBody = Expression.Call(envParam,
                                               _defineMethod,
                                               [kParam, Expression.Constant(sym), val]);
            } else {
                ParameterExpression pe = lexVars.ParameterForDefine(sym);
                contBody = DynInv(kParam, Expression.Assign(pe, val));
            }
            Expression<CompiledCode> valCC = (Expression<CompiledCode>)Analyze(lexVars, valExpr).Reduce();
            var k = Expression.Lambda(contBody, [val]);
            Body = Expression.Invoke(valCC, [k, envParam]);

        }


        protected override Expression Body {get;}
    }

    internal static Expression DynInv(params Expression[] xs) {
        return Expression.Dynamic(
            binder: new MyInvokeBinder(new CallInfo(xs.Length -1)),
            //TODO: will dynamic invoke always be an Action? should returnType by void?
            returnType: typeof(object),
            arguments: xs);
    }


    private class LambdaExprET : ET {
        static readonly ConstructorInfo procedureCstr = typeof(Procedure).GetConstructor([typeof(Delegate)]) ?? throw new Exception("could not find constructor for Procedure");

        public LambdaExprET(LexicalContext scope, ParsedLambda lambdaExpr) {
            ParsedForm[] lambdaBody = lambdaExpr.Bodies;

            var k = Expression.Parameter(typeof(Delegate), "k in LambdaExprET"); // this is the continuation parameter for the proc we are making
            // TODO: couldn't k be a more specific type than Delegate
            LexicalContext lambdaScope;
            var parameters = lambdaExpr.Parameters;
            var rest = parameters.Rest;
            if (rest is not null) {
                if (parameters.HasRequired) {
                    // parameters are something like (a b . c)
                    IEnumerable<Symbol> symbols = parameters.Required.Select(v => v.Identifier.Symbol).Append(rest.Identifier.Symbol);
                    lambdaScope = scope.Extend(symbols);
                    LambdaExpressionBody = LambdaBody(k, lambdaScope, lambdaBody);
                    LambdaExpressionParams = [k, .. lambdaScope.Parameters];
                    Body = Expression.Convert(DynInv(kParam, // here kParam is the continuation when the lambda expression is being evaluated
                                                Expression.New(procedureCstr,
                                                            [
                                                                Expression.Lambda(
                                                                    delegateType: FunctionTypeFromRequiredCount(parameters.Required.Length),
                                                                    body: LambdaExpressionBody,
                                                                    parameters: LambdaExpressionParams)
                                                            ])), typeof(Thunk));
                } else {
                    // parameters are something like a
                    lambdaScope = scope.Extend([rest.Identifier.Symbol]);
                    LambdaExpressionBody = LambdaBody(k, lambdaScope, lambdaBody);
                    LambdaExpressionParams = [k, .. lambdaScope.Parameters];

                    Body = Expression.Convert(DynInv(kParam, // here kParam is the continuation when the lambda expression is being evaluated
                                                Expression.New(procedureCstr,
                                                            new Expression[] {
                                                                Expression.Lambda(delegateType: typeof(ListFunction),
                                                                body: LambdaExpressionBody,
                                                                parameters: LambdaExpressionParams)
                                                            })), typeof(Thunk));
                }

            } else {
                // parameters are like (a b c)
                lambdaScope = scope.Extend(parameters.Required.Select(i => i.Identifier.Symbol));
                LambdaExpressionBody = LambdaBody(k, lambdaScope, lambdaBody);
                LambdaExpressionParams = new[] {k}.Concat(lambdaScope.Parameters).ToArray();
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
            }
        }

        public LambdaExprET(LexicalContext scope, List.NonEmpty args) {
            IForm lambdaParameters = args.ElementAt(1) is Syntax stx ? Syntax.ToDatum(stx) : args.ElementAt(1); // TODO: do we want to throw this info out already?
            Debug.Assert(args.Count() >= 3);
            List.NonEmpty lambdaBody = (List.NonEmpty)args.Skip(2).ToJigList();

            var k = Parameter(typeof(Delegate), "k in MakeProcET"); // this is the continuation parameter for the proc we are making
            LexicalContext lambdaScope;
            switch (lambdaParameters) {
                case List properList:
                    IEnumerable<Symbol> properListSymbols = properList.Cast<Symbol>() ?? throw new Exception($"malformed lambda: expected parameters to be symbols but got {properList}");
                    lambdaScope = scope.Extend(properListSymbols);
                    LambdaExpressionBody = LambdaBody(k, lambdaScope, lambdaBody);
                    LambdaExpressionParams = new[] {k}.Concat(lambdaScope.Parameters).ToArray();
                    Body =
                        Convert(
                            DynInv(kParam,
                                   New(
                                       procedureCstr, Lambda(body: LambdaExpressionBody,
                                           parameters: LambdaExpressionParams))),
                            typeof(Thunk));
                    return;
                case Symbol onlyRestSymbol: // cases like (lambda x x)
                    lambdaScope = scope.Extend([onlyRestSymbol]);
                    LambdaExpressionBody = LambdaBody(k, lambdaScope, lambdaBody);
                    LambdaExpressionParams = new[] {k}.Concat(lambdaScope.Parameters).ToArray();

                    Body = Convert(DynInv(kParam, // here kParam is the continuation when the lambda expression is being evaluated
                                             New(procedureCstr, Lambda(delegateType: typeof(ListFunction),
                                                                body: LambdaExpressionBody,
                                                                parameters: LambdaExpressionParams))), typeof(Thunk));
                    return;
                case IPair andRest: // cases like (lambda (proc l . ls))
                    IEnumerable<Symbol> symbols = ValidateAndConvertToSymbols(andRest);
                    lambdaScope = scope.Extend(symbols);
                    LambdaExpressionBody = LambdaBody(k, lambdaScope, lambdaBody);
                    LambdaExpressionParams = new[] {k}.Concat(lambdaScope.Parameters).ToArray();
                    Body = Convert(DynInv(kParam, // here kParam is the continuation when the lambda expression is being evaluated
                                             New(procedureCstr, Lambda(delegateType: FunctionTypeFromParameters(andRest),
                                                                body: LambdaExpressionBody,
                                                                parameters: LambdaExpressionParams))), typeof(Thunk));
                    return;

                default:
                    throw new Exception($"in MakeProcET: can't handle parameters with type {lambdaParameters.GetType()} yet.");
            }

        }

        private Expression LambdaExpressionBody {get;}

        private ParameterExpression[] LambdaExpressionParams {get;}

        protected override Expression Body {get;}

        private static System.Collections.Generic.List<Symbol> ValidateAndConvertToSymbols(IPair parameters) {
            var result = new System.Collections.Generic.List<Symbol>();
            IForm car = parameters.Car;
            Symbol sym = car is Syntax.Identifier id ? id.Symbol : car is Symbol s ? s : throw new Exception($"lambda: all parameters must be symbols (given {car}");
            result.Add(sym);
            IForm cdr = parameters.Cdr;
            while (cdr is IPair pairCdr) {
                car = pairCdr.Car;
                sym = car is Syntax.Identifier i ? i.Symbol : car is Symbol sm ? sm : throw new Exception($"lambda: all parameters must be symbols (given {car}");
                result.Add(sym);
                cdr = pairCdr.Cdr;
            }
            sym = cdr is Syntax.Identifier idf ? idf.Symbol : cdr is Symbol symbol ? symbol : throw new Exception($"lambda: all parameters must be symbols (given {cdr})");
            result.Add(sym);
            return result;

        }

        private static Type FunctionTypeFromRequiredCount(int num) {
            return num switch
            {
                0 => throw new Exception("In FunctionTypeFromParameters: found 0 parameters before the rest parameter, but there should be at least one"),
                1 => typeof(PairFunction),
                2 => typeof(ImproperListFunction2),
                3 => typeof(ImproperListFunction3),
                4 => typeof(ImproperListFunction4),
                5 => typeof(ImproperListFunction5),
                6 => typeof(ImproperListFunction6),
                7 => typeof(ImproperListFunction7),
                _ => throw new Exception("lambda: can't handle improper codes parameters with more than 7 parameters before the rest parameter."),
            };
        }

        private static Type FunctionTypeFromParameters(IPair parameters) {
            // how many parameters before rest parameter?
            int num = 1;
            object cdr = parameters.Cdr;
            while (cdr is IPair pairCdr) {
                num ++;
                cdr = pairCdr.Cdr;
            }
            return FunctionTypeFromRequiredCount(num);
        }

        private InvocationExpression LambdaBody(ParameterExpression k, LexicalContext scope, List.NonEmpty exprs) {
            // TODO: why is this so much more complicated than just returning a block expr?
            // TODO: shouldn't this create a Block?

            // k is the parameter continuation for the procedure that is being made (the continuation when that proc is applied)
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
                    parameters: [cont]),
                new Expression[] {k}
            );
        }
        private InvocationExpression LambdaBody(ParameterExpression k, LexicalContext scope, ParsedForm[] exprs) {
            // TODO: why is this so much more complicated than just returning a block expr?
            // TODO: shouldn't this create a Block?

            // k is the parameter continuation for the procedure that is being made (the continuation when that proc is applied)
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
                    parameters: [cont]),
                new Expression[] {k}
            );
        }
    }

    private class BlockET : ET {

        public BlockET(LexicalContext scope, List.NonEmpty exprs)
        {
            LexicalContext blockScope = scope.Extend();
            // analyze and reduce all but last expr
            var analyzed = exprs
                .Take(exprs.Count() - 1)
                .Select(x => (Expression<CompiledCode>)Analyze(blockScope, x).Reduce()).ToArray<Expression>();
            // forcing to array to deal with weird bug where things were done out of order when handled as IEnumerable
            Expression<CompiledCode> lastExprCC = (Expression<CompiledCode>)Analyze(blockScope, exprs.Last()).Reduce();
            Expression thunkExpr = Expression.Lambda<Thunk>(Expression.Convert(Expression.Invoke(lastExprCC, kParam, envParam), typeof(Thunk)));
            Expression<CompiledCode> last = Expression.Lambda<CompiledCode>(thunkExpr, kParam, envParam);
            analyzed = analyzed.ToList().Append(last).ToArray();
            Body = Expression.Block(
                blockScope.Parameters,
                new Expression[] {
                    Expression.Invoke(
                        Expression.Constant((Func<Delegate, IEnvironment, CompiledCode[], int, Thunk>) doSequence),
                        kParam,
                        envParam,
                        Expression.NewArrayInit(typeof(CompiledCode), analyzed),
                        Expression.Constant(0))
                });
        }
        public BlockET(LexicalContext scope, ParsedForm[] exprs)
        {
            LexicalContext blockScope = scope.Extend();
            // analyze and reduce all but last expr
            var analyzed = exprs
                .Take<Syntax>(exprs.Count<Syntax>() - 1)
                .Select(x => (Expression<CompiledCode>)Analyze(blockScope, x).Reduce()).ToArray<Expression>();
            // forcing to array to deal with weird bug where things were done out of order when handled as IEnumerable
            Expression<CompiledCode> lastExprCC = (Expression<CompiledCode>)Analyze(blockScope, (ParsedForm)exprs.Last<Syntax>()).Reduce();
            Expression thunkExpr = Expression.Lambda<Thunk>(Expression.Convert(Expression.Invoke(lastExprCC, kParam, envParam), typeof(Thunk)));
            Expression<CompiledCode> last = Expression.Lambda<CompiledCode>(thunkExpr, kParam, envParam);
            analyzed = [.. analyzed, last];
            Body = Expression.Block(
                blockScope.Parameters,
                new Expression[] {
                    Expression.Invoke(
                        Expression.Constant((Func<Delegate, IEnvironment, CompiledCode[], int, Thunk>) doSequence),
                        kParam,
                        envParam,
                        Expression.NewArrayInit(typeof(CompiledCode), analyzed),
                        Expression.Constant(0))
                });
        }

        internal static Thunk doSequence(Delegate k, IEnvironment env, CompiledCode[] codes, int index) {
            var code = codes[index];
            var cont = index == codes.Length - 1 ? k : (Continuation.ContinuationAny)K0;
            return code(cont, env);
            Thunk? K0(IForm[] _) => doSequence(k, env, codes, index + 1);
        }

        protected override Expression Body {get;}
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

    private static Expression EnsureObjectResult (Expression expr) {
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

    private static BindingRestrictions GetTargetArgsRestrictions(
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
        foreach (var t in args) {
            BindingRestrictions r;
            if (t is { HasValue: true, Value: null }) {
                r = BindingRestrictions.GetInstanceRestriction(
                    t.Expression, null);
            } else {
                r = BindingRestrictions.GetTypeRestriction(
                    t.Expression, t.LimitType);
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

    private static CallSignature CallInfoToSignature(CallInfo callInfo) {
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
