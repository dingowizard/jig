using System.Diagnostics;
using System.Linq.Expressions;
using System.Reflection;
using Jig;
using Jig.Expansion;
using Jig.Types;
using Expression = System.Linq.Expressions.Expression;
using String = Jig.String;
namespace VM;

public class InterOp {

    public static InterOp GlobalInterOp; //TODO: UGH!!!!
    
    public TypeResolver TypeResolver {get;}
    
    public InterOp(TypeResolver resolver, Evaluator ev) {
        TypeResolver = resolver;
        Evaluator = ev;
    }
    public Evaluator Evaluator {get;}


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

    public Library ImportClrNameSpace(string name) {
        Type[] typesWeKnow = [typeof(int), typeof(double), typeof(string), typeof(bool), typeof(char), typeof(char[])];
        var ts = ResolveClrName(name);
        
        // for now, we're going to select only those static methods that receive double or int arguments and return double results
        // get const double fields (PI and E)
        // Console.WriteLine($"Importing {name}. {name} has {ts.Count()} types: {string.Join(", ", ts)} ");
        // foreach (var memberInfos in ts.SelectMany(t => t.GetMembers()).GroupBy(memberInfo => memberInfo.GetType())) {
        //     foreach (var mi in memberInfos) {
        //         // Console.WriteLine($"{mi.Name} is a {mi.GetType()}");
        //         // if (mi is MethodInfo method) {
        //         //     Console.WriteLine($"\t{method.Name}({string.Join(", ", method.GetParameters().Select(p => p.ParameterType.Name))}) -> {method.ReturnType}");
        //         // }
        //         if (mi is PropertyInfo property) {
        //             var accessors = property.GetAccessors();
        //             Console.WriteLine($"property has {accessors.Length} accessors");
        //             foreach (var accessor in accessors) {
        //                 Console.WriteLine($"\taccessor has {accessor.GetParameters().Length}: {string.Join(", ", accessor.GetParameters().Select(p => p.ParameterType.Name))}");
        //             }
        //         }
        //     }
        // }
        
        var constantFields =
            ts.SelectMany(t => t.GetFields(BindingFlags.Public | BindingFlags.Static))
                .Where(fi => (fi.IsLiteral || fi.IsInitOnly) && typesWeKnow.Contains(fi.FieldType));
        System.Collections.Generic.List<Binding> bindings = [];
        int index = 0;
        foreach (var f in constantFields) {
            SchemeValue schemeValue = TypeResolver.Resolve(f.FieldType).WrapReturn(f.IsLiteral ? f.GetRawConstantValue() : f.GetValue(null));

            string fullName =  f.DeclaringType.Name +  "." +   f.Name;
            // Console.WriteLine($"field = {fullName}");
            var bg = new Binding(new Jig.Expansion.Parameter(new Symbol(fullName), [], 0, index++, null),
                new Location(schemeValue));
            bindings.Add(bg);
        }
        
        var staticMethodInfos =
            ts.SelectMany(t => t.GetMethods(BindingFlags.Public | BindingFlags.Static))
                .Where(mi => typesWeKnow.Contains(mi.ReturnType) && mi.GetParameters().All(p => typesWeKnow.Contains(p.ParameterType)))
                .GroupBy(mi => mi.Name);
        foreach (var methodGroup in staticMethodInfos) {
            var arr = methodGroup.ToArray();
            string fullName = arr[0].DeclaringType.Name + "."  +  arr[0].Name; 
            // Console.WriteLine($"trying to import {fullName}");
            Procedure clrProcedure = ProcedureFromMethodGroup(arr);
            var bg = new Binding(
                new Jig.Expansion.Parameter(new Symbol(fullName), [], 0, index++, null),
                new Location(clrProcedure));
            // Console.WriteLine($"adding {fullName}");
            bindings.Add(bg);
        }
        
        // TODO: we can get rid of these type filters when the type resolver knows what to do with generics and interfaces
        var instanceMethodGroups =
            ts.SelectMany(t => t.GetMethods(BindingFlags.Public | BindingFlags.Instance))
                .Where(mi => typesWeKnow.Contains(mi.DeclaringType) &&
                             typesWeKnow.Contains(mi.ReturnType) &&
                             mi.GetParameters().All(p => typesWeKnow.Contains(p.ParameterType)))
                .Where(mi => !mi.IsSpecialName)
                .GroupBy(mi => mi.Name);
        foreach (var methodGroup in instanceMethodGroups) {
            var arr = methodGroup.ToArray();
            string fullName = arr[0].DeclaringType.Name + "."  +  arr[0].Name; 
            Procedure clrProcedure = ProcedureFromMethodGroup(arr);
            var bg = new Binding(
                new Jig.Expansion.Parameter(new Symbol(fullName), [], 0, index++, null),
                new Location(clrProcedure));
            bindings.Add(bg);
            
        }
        var instanceProperties =
            ts.SelectMany(t => t.GetProperties(BindingFlags.Public | BindingFlags.Instance))
                .Where(p => typesWeKnow.Contains(p.PropertyType));
        foreach (var prop in instanceProperties) {
            var propertyName = prop.DeclaringType.Name + "." + prop.Name;
            var procedure = ProcedureFromInstanceProperty(prop);
            var bg = new Binding(
                new Jig.Expansion.Parameter(new Symbol(propertyName), [], 0, index++, null),
                new Location(procedure));
            bindings.Add(bg);
        }
        var staticProperties =
            ts.SelectMany(t => t.GetProperties(BindingFlags.Public | BindingFlags.Static))
                .Where(p => typesWeKnow.Contains(p.PropertyType));
        foreach (var prop in staticProperties) {
            
        }
        var ctors =
            ts.SelectMany(t => t.GetConstructors(BindingFlags.Public | BindingFlags.Instance))
                .Where(ci => typesWeKnow.Contains(ci.DeclaringType)).ToArray();
        // var ctorProc = ProcedureFromMethodGroup(ctors);
        // var bindingForCtor = new Binding(
        //     new Jig.Expansion.Parameter(new Symbol(ctors[0].DeclaringType.Name + ".new"), [], 0, index++, null),
        //     new Location(ctorProc));
        // bindings.Add(bindingForCtor);
        return new Library(bindings, []);

    }

    private string MethodName(MethodInfo mi) {
        
        System.Collections.Generic.List<string> paramNameAndTypes = [];
        foreach (var p in mi.GetParameters()) {
            paramNameAndTypes.Add(p.ParameterType.Name);
        }
        string ps = string.Join("->", paramNameAndTypes);
        string returnType = mi.ReturnType.Name;
        return mi.DeclaringType.Name +  "."  + mi.Name + "/" + ps + (ps != "" ? "->" + returnType : returnType);
    }

    private Procedure MakeProcedure(ParsedLambda.LambdaParameters lambdaParams, ParsedForm[] body, string name) {
        var parsedLambda = new ParsedLambda(
            new Identifier(new Symbol("lambda")),
            lambdaParams,
            lambdaParams.Required.Length + (lambdaParams.HasRest ? 1 : 0),
            body);
        // Console.WriteLine($"{Syntax.ToDatum(parsedLambda).Print()}");
        var compiler = new Compiler();
        var template = compiler.CompileLambdaTemplate(parsedLambda, Environment.Default, 0);
        template.Name = new Identifier(new Symbol(name));
        return new Procedure(Evaluator.Environment, template);
        
    }

    public Procedure ProcedureFromMethodGroup(ConstructorInfo[] constructorInfos) {
        
        if (HaveSameNumberParameters(constructorInfos)) {
            ParsedLambda.LambdaParameters lambdaParameters = LambdaParametersForConstructor(constructorInfos[0]);
            System.Collections.Generic.List<(ParsedForm @if, ParsedApplication call)> stmts = [];
            foreach (var ci in constructorInfos) {
                stmts.Add( ConditionAndCallForOverride(ci, lambdaParameters));
            }
            return MakeProcedure(lambdaParameters, BodyForMethodGroup(constructorInfos, stmts), constructorInfos[0].DeclaringType + ".new");
        }
        throw new NotImplementedException();
        // return ProcedureFromMethodsVariousParamLengths(constructorInfos);
        
    }
    
    public Procedure ProcedureFromMethodGroup(MethodInfo[] methodInfos) {
        
        if (HaveSameNumberParameters(methodInfos)) {
            
            ParsedLambda.LambdaParameters lambdaParameters = LambdaParametersForMethodGroupSameParamNum(methodInfos);
            System.Collections.Generic.List<(ParsedForm @if, ParsedApplication call)> stmts = [];
            foreach (var m in methodInfos) {
                stmts.Add( ConditionAndCallForOverride(m, lambdaParameters));
            }
            return MakeProcedure(lambdaParameters, BodyForMethodGroup(methodInfos, stmts), methodInfos[0].Name);
        }
        return ProcedureFromMethodsVariousParamLengths(methodInfos);
    }
    private Procedure ProcedureFromMethodsVariousParamLengths(MethodInfo[] methodInfos) {
        // TODO: need to deal with methods that have a params parameter at end
        var sorted = methodInfos.OrderBy(mi => mi.GetParameters().Length);
        var numRequired = sorted.ElementAt(0).GetParameters().Length + (methodInfos[0].IsStatic ? 0 : 1);
        System.Collections.Generic.List<Parameter> parameters = [];
        int i = 0;
        for (; i < numRequired; i++) {
            parameters.Add(new Parameter(new Symbol($"x{i}"),  [], 1, i, null));
            // TODO: when we build the lambda expressions that handle each smae-parameter-length group of methods, we'll need to increment the scope level
        }
        // add the rest parameter
        var rest = new Parameter(new Symbol("xs"), [], 1, i, null);
        SchemeValue expr = rest;
        for (int n = parameters.Count - 1; n >= 0; n--) {
            expr = (SchemeValue)Pair.Cons(parameters[n], expr);
        }
        var lambdaParams = new ParsedLambda.LambdaParameters(new Syntax(expr), parameters.ToArray(), rest);
        return MakeProcedure(lambdaParams, BodyForMethodGroupVariousParamLengths(numRequired, lambdaParams, sorted), methodInfos[0].Name);
    }
    private ParsedForm[] BodyForMethodGroupVariousParamLengths(int shortest, ParsedLambda.LambdaParameters lambdaParams, IOrderedEnumerable<MethodInfo> methodInfos) {
        var mi = methodInfos.ElementAt(0);

        // Console.WriteLine($"Making procedure for {mi.Name}. The override with the least parameters has {shortest} parameters.");
        // Console.WriteLine($"\tthe lambda parameters for the function are: {Syntax.E(lambdaParams).Print()}. required = {lambdaParams.Required.Length}");
        return [BodyExprMethodGroupVariousLengths(shortest, lambdaParams, methodInfos, mi.Name, mi.DeclaringType)];
    }
    private ParsedForm BodyExprMethodGroupVariousLengths(int shortest, ParsedLambda.LambdaParameters lambdaParams, IOrderedEnumerable<MethodInfo> methodInfos, string name, Type? miDeclaringType) {
        if (methodInfos.Count() == 0) {
            return CouldNotResolveErrorCall(name, miDeclaringType);
        }
        int n = methodInfos.ElementAt(0).GetParameters().Length + (methodInfos.ElementAt(0).IsStatic ? 0 : 1);
        System.Collections.Generic.List<MethodInfo> shortestMethods = [];
        System.Collections.Generic.List<MethodInfo> rest = [];
        foreach (var mi in methodInfos) {
            if ((mi.GetParameters().Length + (mi.IsStatic ? 0 : 1)) == n) {
                shortestMethods.Add(mi);
            } else {
                rest.Add(mi);
            }
        }
        var cond = ConditionForMethodGroupParamLength(shortest, shortestMethods[0].GetParameters().Length + (shortestMethods[0].IsStatic ? 0 : 1), lambdaParams.Rest);
        var then = ThenBranchForMethodGroupVariousLengths(lambdaParams, shortestMethods, lambdaParams);
        var @else = BodyExprMethodGroupVariousLengths(shortest, lambdaParams, rest.OrderBy(mi => mi.GetParameters().Length), name, miDeclaringType); 
        return new ParsedIf(
            new Identifier(new Symbol("if")),
            cond,
            then,
            @else);
    }
    private ParsedForm ThenBranchForMethodGroupVariousLengths(ParsedLambda.LambdaParameters lambdaParams, System.Collections.Generic.List<MethodInfo> shortestMethods, ParsedLambda.LambdaParameters lambdaParameters) {
        // this has to make something like:
        // ((lambda (x0 x1 x2 x3) ...)
        //  x0 x1 (car xs) (car (cdr xs)))
        // assuming there were two required args and a rest parameter
        // we should be able to reuse some code from where we made a procedure for method groups of same parameter length
        int numRestArgs = (shortestMethods[0].GetParameters().Count() + (shortestMethods[0].IsStatic ? 0 : 1)) - lambdaParameters.Required.Length;
        var proc = ProcedureFromMethodGroup(shortestMethods.ToArray());
        var requiredArgs = lambdaParams.Required.Select(p => new ParsedVariable.Lexical(new Identifier(p.Symbol), p, null));
        var restArgs = RestArgs(numRestArgs, lambdaParams.Rest);
        return new ParsedApplication(
            ((ParsedForm[])
            [
                new ParsedLiteral(
                    new Identifier(new Symbol("quote")),
                    new Syntax.Literal(proc)),
            ]).Concat(requiredArgs)
            .Concat(restArgs), null);
    }
    private IEnumerable<ParsedForm> RestArgs(int numArgs, Parameter? lambdaParamsRest) {
        if (numArgs == 0) return [];
        return Enumerable.Range(1, numArgs).Select(i => RestArg(i, lambdaParamsRest));
    }
    private ParsedForm RestArg(int i, Parameter? lambdaParamsRest) {
        var carParam = Library.Core.FindParameter("car");
        return new ParsedApplication(new []
        {
            new ParsedVariable.TopLevel(new Identifier(carParam.Symbol), carParam, null),
            RestArgsCdr(i - 1, lambdaParamsRest),
            
        }, null);
    }
    private ParsedForm RestArgsCdr(int i, Parameter? lambdaParamsRest) {
        if (i == 0) return new ParsedVariable.Lexical(new Identifier(lambdaParamsRest.Symbol), lambdaParamsRest, null);
        return new ParsedApplication(new []
        {
            new ParsedVariable.TopLevel(new Identifier(new Symbol("cdr")), Library.Core.FindParameter("cdr"), null),
            RestArgsCdr(i - 1, lambdaParamsRest),
            
        }, null);
    }

    private ParsedForm ConditionForMethodGroupParamLength(int shortest, int length, Parameter? restParameter) {
        int l = length - shortest;
        // TODO: overload ParsedApplication with a params ParsedForm[] arg
        var lengthOfXs = new ParsedApplication(
        [
            new ParsedVariable.TopLevel(new Identifier(new Symbol("length")), Library.Core.FindParameter("length"), null),
            new ParsedVariable.Lexical(new Identifier(restParameter.Symbol),  restParameter, null),
        ], null);
        return new ParsedApplication(
            [
                new ParsedVariable.TopLevel(new Identifier(new Symbol("=")), Library.Core.FindParameter("="), null),
                new ParsedLiteral(new Identifier(new Symbol("quote")), new Syntax.Literal(new Integer(l))),
                lengthOfXs,
            ], null);
    }

    private ParsedForm[] BodyForMethodGroup(MethodBase[] methodInfos, System.Collections.Generic.List<(ParsedForm @if, ParsedApplication call)> stmts) {
        return [NestedIfsForMethodGroup(methodInfos, stmts)];
    }
    private ParsedForm NestedIfsForMethodGroup(MethodBase[] methodInfos, IEnumerable<(ParsedForm @if, ParsedApplication call)> stmts) {
        if (stmts.Count() == 1) {
            return new ParsedIf(
                new Identifier(new Symbol("if")),
                stmts.ElementAt(0).@if,
                stmts.ElementAt(0).call,
                CouldNotResolveErrorCall(methodInfos[0].Name, methodInfos[0].DeclaringType));
        }
        return new ParsedIf(
            new Identifier(new Symbol("if")),
            stmts.ElementAt(0).@if,
            stmts.ElementAt(0).call,
            NestedIfsForMethodGroup(methodInfos, stmts.Skip(1)));
    }
    
    private (ParsedForm test, ParsedApplication call) ConditionAndCallForOverride(ConstructorInfo ctorInfo, ParsedLambda.LambdaParameters lambdaParameters) {
        ParsedForm condition = ConditionForOverrideAux(ctorInfo.GetParameters(), lambdaParameters.Required.ToList());
        ParsedApplication app = CallForOverride(ctorInfo, lambdaParameters);
        return (condition, app);

    }
    
    private (ParsedForm test, ParsedApplication call) ConditionAndCallForOverride(MethodInfo methodInfo, ParsedLambda.LambdaParameters lambdaParameters) {
        ParsedForm condition = ConditionForOverride(methodInfo, methodInfo.GetParameters().ToList(), lambdaParameters.Required.ToList());
        ParsedApplication app = CallForOverride(methodInfo, lambdaParameters);
        return (condition, app);

    }
    private ParsedForm ConditionForOverride(MethodInfo methodInfo, System.Collections.Generic.List<ParameterInfo> toList, System.Collections.Generic.List<Parameter> parameters) {
        if (methodInfo.IsStatic) {
            return ConditionForOverrideAux(toList, parameters);
        } else {
            return new ParsedIf(
                new Syntax(new Symbol("if")),
                PredicateApplicationFromType(methodInfo.DeclaringType, new ParsedVariable.Lexical(new Identifier(parameters.ElementAt(0).Symbol), parameters.ElementAt(0), null)),
                ConditionForOverrideAux(toList, parameters.Skip(1)),
                new ParsedLiteral(new Syntax(new Symbol("quote")), new Syntax.Literal(Bool.False)));
        }
    }
    private ParsedForm ConditionForOverrideAux(IEnumerable<ParameterInfo> parameterInfos, IEnumerable<Parameter> parameters) {
        
        Debug.Assert(parameterInfos.Count() == parameters.Count());
        if (parameters.Count() == 0) return new ParsedLiteral(new Syntax(new Symbol("quote")), new Syntax.Literal(Bool.True));
        return new ParsedIf(
            new Syntax(new Symbol("if")),
            PredicateApplicationFromParameterInfo(parameterInfos.ElementAt(0), new ParsedVariable.Lexical(new Identifier(parameters.ElementAt(0).Symbol), parameters.ElementAt(0), null)),
            ConditionForOverrideAux(parameterInfos.Skip(1), parameters.Skip(1)),
            new ParsedLiteral(new Syntax(new Symbol("quote")), new Syntax.Literal(Bool.False)));
    }
    
    private ParsedApplication CallForOverride(ConstructorInfo constructorInfo, ParsedLambda.LambdaParameters lambdaParameters) {
        var clrMethod = new ClrPrimitive(constructorInfo, TypeResolver);
        ParsedForm[] forms = [new ParsedLiteral(new Identifier(new Symbol("quote")), new Syntax.Literal(clrMethod), null)];
        var args = lambdaParameters.Required.Select(p => new ParsedVariable.Lexical(new Identifier(p.Symbol), p, null));
        return new ParsedApplication(forms.Concat(args), null);
    }
    private ParsedApplication CallForOverride(MethodInfo methodInfo, ParsedLambda.LambdaParameters lambdaParameters) {
        var clrMethod = new ClrPrimitive(methodInfo, TypeResolver);
        ParsedForm[] forms = [new ParsedLiteral(new Identifier(new Symbol("quote")), new Syntax.Literal(clrMethod), null)];
        var args = lambdaParameters.Required.Select(p => new ParsedVariable.Lexical(new Identifier(p.Symbol), p, null));
        return new ParsedApplication(forms.Concat(args), null);
    }
    private ParsedLambda.LambdaParameters LambdaParametersForMethodGroupSameParamNum(MethodInfo[] methodInfos) {
        Debug.Assert(HaveSameNumberParameters(methodInfos));
        return LambdaParametersForMethod(methodInfos[0]);
    }

    private ParsedLambda.LambdaParameters LambdaParametersForConstructor(ConstructorInfo constructorInfo) {
        return LambdaParametersFromParameterInfos(constructorInfo.GetParameters(), 0);
    }
    
    private ParsedLambda.LambdaParameters LambdaParametersForMethod(MethodInfo methodInfo) {
        if (methodInfo.IsStatic) {
            return LambdaParametersFromParameterInfos(methodInfo.GetParameters(), 0);
        } else {
            var ps = LambdaParametersFromParameterInfos(methodInfo.GetParameters(), 1);
            Identifier objParam = new Identifier(new Symbol("arg0"));
            var parameter = new Jig.Expansion.Parameter(objParam.Symbol, [], 1, 0, null);
            return new ParsedLambda.LambdaParameters(
                new Syntax((SchemeValue)Pair.Cons(objParam, (SyntaxList)Syntax.E(ps)), null),
                new[]
                {
                    parameter
                }.Concat(ps.Required).ToArray(),
                ps.Rest);
        }
    }
    
    private bool HaveSameNumberParameters(MethodBase[] methodInfos) {
        int n = methodInfos[0].GetParameters().Length;
        foreach (var m in methodInfos.Skip(1)) {
            if (n != m.GetParameters().Length) {
                return false;
            }
        }
        return true;
    }

    public Procedure ProcedureFromInstanceProperty(PropertyInfo prop) {
        var lambdaParameters = LambdaParametersFromInstanceProperty(prop);
        return MakeProcedure(lambdaParameters, LambdaBodyForInstanceMethod(prop.GetGetMethod(), lambdaParameters), prop.DeclaringType.Name + "." + prop.Name);
        // return MakeProcedure(lambdaParameters, LambdaBodyForInstanceProperty(prop, lambdaParameters), prop.DeclaringType.Name + "." + prop.Name);
    }

    public Procedure ProcedureFromInstanceMethodInfo(MethodInfo methodInfo) {
        var lambdaParameters = LambdaParametersForInstanceMethod(methodInfo);
        return MakeProcedure(lambdaParameters, LambdaBodyForInstanceMethod(methodInfo, lambdaParameters), MethodName(methodInfo));
    }

    public Procedure ProcedureFromStaticMethodInfo(MethodInfo methodInfo) {
        // Let's build a ParsedLambda
        // and then compile it.
        var lambdaParameters = LambdaParametersFromParameterInfos(methodInfo.GetParameters());
        return MakeProcedure(lambdaParameters, LambdaBodyFromMethodInfo(methodInfo, lambdaParameters), MethodName(methodInfo));
    }
    
    private ParsedForm IfExprForObjectArg(string name, Type declaringType, ParsedVariable.Lexical objectArg) {
        ParsedApplication predApp = PredicateApplicationFromType(declaringType, objectArg);
        ParsedApplication errorCall = WrongArgTypeErrorCall(name, declaringType, objectArg);
        return new ParsedIf(new Identifier(new Symbol("if")), predApp, objectArg, errorCall);
    }
    
    private ParsedApplication CouldNotResolveErrorCall(string name, Type declaringType) {
        Jig.Expansion.Parameter errorParameter = Library.Core.FindParameter("error");
        var errorVar = new ParsedVariable.TopLevel(new Identifier(errorParameter.Symbol), errorParameter, null);
        var errorMsg = new String($"could not resolve override for method {declaringType.Name}.{name}");
        return new ParsedApplication(
            [
                errorVar,
                new ParsedLiteral(new Identifier(new Symbol("quote")), new Identifier(new Symbol(name))),
                new ParsedLiteral(new Identifier(new Symbol("quote")), new Syntax.Literal(errorMsg)),
                ],
            null);
    }
    private ParsedApplication WrongArgTypeErrorCall(string name, Type declaringType, ParsedVariable.Lexical parsedVariable) {
        Jig.Expansion.Parameter errorParameter = Library.Core.FindParameter("error");
        var errorVar = new ParsedVariable.TopLevel(new Identifier(errorParameter.Symbol), errorParameter, null);
        var errorMsg = new String($"expected argument for parameter {declaringType.Name.ToLower()} to be convertible to clr type {declaringType.Name}.");
        return new ParsedApplication(
        [
            errorVar,
            new ParsedLiteral(new Identifier(new Symbol("quote")), new Identifier(new Symbol(name))),
            new ParsedLiteral(new Identifier(new Symbol("quote")), new Syntax.Literal(errorMsg)),
            parsedVariable, ],
            null);
    }
    private ParsedIf IfExprFromParameterInfo(MethodInfo methodInfo, ParameterInfo propertyInfo, ParsedVariable.Lexical arg) {

        ParsedApplication predApp = PredicateApplicationFromParameterInfo(propertyInfo, arg);
        ParsedApplication errorCall = WrongArgTypeErrorCall(methodInfo, propertyInfo, arg);
        return new ParsedIf(new Identifier(new Symbol("if")), predApp, arg, errorCall);
    }
    private ParsedApplication WrongArgTypeErrorCall(MethodInfo methodInfo, ParameterInfo parameterInfo, ParsedVariable.Lexical parsedVariable) {
        Jig.Expansion.Parameter errorParameter = Library.Core.FindParameter("error");
        var errorVar = new ParsedVariable.TopLevel(new Identifier(errorParameter.Symbol), errorParameter, null);
        var errorMsg = new String($"expected argument for parameter {parameterInfo.Name} to be convertible to clr type {parameterInfo.ParameterType.Name}.");
        return new ParsedApplication(
        [
            errorVar,
            new ParsedLiteral(new Identifier(new Symbol("quote")), new Identifier(new Symbol(methodInfo.Name))),
            new ParsedLiteral(new Identifier(new Symbol("quote")), new Syntax.Literal(errorMsg)),
            parsedVariable, ],
            null);
    }
    private ParsedApplication PredicateApplicationFromParameterInfo(ParameterInfo parameterInfo, ParsedVariable.Lexical arg) {
        Primitive pred = Primitive.TypePredicate(TypeResolver.Resolve(parameterInfo.ParameterType));
        return new ParsedApplication([new ParsedLiteral(new Identifier(new Symbol("quote")), new Syntax.Literal(pred), null), arg], null);
    }
    
    private ParsedApplication PredicateApplicationFromType(Type type, ParsedVariable.Lexical arg) {
        Primitive pred = Primitive.TypePredicate(TypeResolver.Resolve(type));
        return new ParsedApplication([new ParsedLiteral(new Identifier(new Symbol("quote")), new Syntax.Literal(pred), null), arg], null);
    }
    
    private ParsedForm[] LambdaBodyForInstanceProperty(PropertyInfo prop, ParsedLambda.LambdaParameters lambdaParameters) {
        var firstParam = lambdaParameters.Required[0];
        ParsedVariable.Lexical objectArg = new ParsedVariable.Lexical(new Identifier(firstParam.Symbol), firstParam, null);
        System.Collections.Generic.List<ParsedForm> ifs = [IfExprForObjectArg(prop.Name, prop.DeclaringType, objectArg)];
        var clrMethod = new ClrPrimitive(prop.GetMethod, TypeResolver);
        ParsedForm[] forms = [new ParsedLiteral(new Identifier(new Symbol("quote")), new Syntax.Literal(clrMethod), null)];
        var call = new ParsedApplication(forms.Concat(ifs), null);
        // Console.WriteLine($"{Syntax.ToDatum(call).Print()}");
        return [call];
    }
    
    private ParsedForm[] LambdaBodyForInstanceMethod(MethodInfo methodInfo, ParsedLambda.LambdaParameters lambdaParameters) {
        var firstParam = lambdaParameters.Required[0];
        ParsedVariable.Lexical objectArg = new ParsedVariable.Lexical(new Identifier(firstParam.Symbol), firstParam, null);
        System.Collections.Generic.List<ParsedForm> ifs = [IfExprForObjectArg(methodInfo.Name, methodInfo.DeclaringType, objectArg)];
        foreach (var (pInfo, p) in methodInfo.GetParameters().Zip(lambdaParameters.Required.Skip(1))) {
            var arg = new ParsedVariable.Lexical(new Identifier(p.Symbol), p, null);
            var @if = IfExprFromParameterInfo(methodInfo, pInfo, arg);
            // Console.WriteLine($"for {methodInfo.Name} made if: {Syntax.ToDatum(@if).Print()}");
            ifs.Add(IfExprFromParameterInfo(methodInfo, pInfo, arg));
        }
        if (lambdaParameters.HasRest) {
            // we need to do the equivalent of (all can-be-clr-type? rest)
            // and error should be something like: expected all elements of rest arg "r" to be convertible to clr type {tgpe}
            throw new NotImplementedException("we don't know how to import clr methods with params array yet.");
        }
        
        var clrMethod = new ClrPrimitive(methodInfo, TypeResolver);
        ParsedForm[] forms = [new ParsedLiteral(new Identifier(new Symbol("quote")), new Syntax.Literal(clrMethod), null)];
        var call = new ParsedApplication(forms.Concat(ifs), null);
        // Console.WriteLine($"{Syntax.ToDatum(call).Print()}");
        return [call];
    }
    private ParsedForm[] LambdaBodyFromMethodInfo(MethodInfo methodInfo, ParsedLambda.LambdaParameters lambdaParameters) {
        // we need to build a big call something like:
        // (clr-method (if (can-be-clr-type0? arg0) arg0 (error ...)) (if (can-be-clr-type1? arg1) arg1 (error ...)) ... (if (can-be-clr-typen? argn) argn (error ...)))
        // Very unusually, the operator, clr-method, is going to be a literal
        System.Collections.Generic.List<ParsedVariable.Lexical> args = [];
        System.Collections.Generic.List<ParsedForm> ifs = [];
        foreach (var (pInfo, p) in methodInfo.GetParameters().Take(lambdaParameters.Required.Length).Zip(lambdaParameters.Required)) {
            var arg = new ParsedVariable.Lexical(new Identifier(p.Symbol), p, null);
            ifs.Add(IfExprFromParameterInfo(methodInfo, pInfo, arg));
        }
        if (lambdaParameters.HasRest) {
            // we need to do the equivalent of (all can-be-clr-type? rest)
            // and error should be something like: expected all elements of rest arg "r" to be convertible to clr type {tgpe}
            throw new NotImplementedException("we don't know how to import clr methods with params array yet.");
        }
        
        var clrMethod = new ClrPrimitive(methodInfo, TypeResolver);
        ParsedForm[] forms = [new ParsedLiteral(new Identifier(new Symbol("quote")), new Syntax.Literal(clrMethod), null)];
        return [new ParsedApplication(forms.Concat(ifs), null)];
    }
    
    
    private ParsedLambda.LambdaParameters LambdaParametersFromInstanceProperty(PropertyInfo prop) {
        // Identifier objParam = new Identifier(new Symbol(prop.DeclaringType.Name.ToLower()));
        // var parameter = new Jig.Expansion.Parameter(objParam.Symbol, [], 1, 0, null);
        // // Properties can in cact take arguments, so this is not right
        // return new ParsedLambda.LambdaParameters(
        //     new Syntax((SchemeValue)Pair.Cons(objParam, SyntaxList.Null), null),
        //     [ parameter ],
        //     null);
        var methodInfo = prop.GetGetMethod();
        return LambdaParametersForInstanceMethod(methodInfo);
    }
    
    private ParsedLambda.LambdaParameters LambdaParametersForInstanceMethod(MethodInfo methodInfo) {
        var ps = LambdaParametersFromParameterInfos(methodInfo.GetParameters(), 1);
        Identifier objParam = new Identifier(new Symbol(methodInfo.DeclaringType.Name.ToLower()));
        var parameter = new Jig.Expansion.Parameter(objParam.Symbol, [], 1, 0, null);
        return new ParsedLambda.LambdaParameters(
            new Syntax((SchemeValue)Pair.Cons(objParam, (SyntaxList)Syntax.E(ps)), null),
            new[]
            {
                parameter
            }.Concat(ps.Required).ToArray(),
            ps.Rest);
    }
    private ParsedLambda.LambdaParameters LambdaParametersFromParameterInfos(ParameterInfo[] parameters, int offset = 0) {
        if (parameters.Length != 0) {

            ParameterInfo last = parameters[^1];
            SchemeValue syntaxes = SyntaxList.Null;
            int lastIndex = parameters.Length - 1;
            Jig.Expansion.Parameter? rest  = null;
            if (last.IsDefined(typeof(ParamArrayAttribute), false)) {
                Identifier restID = new Identifier(new Symbol(last.Name ?? $"arg{parameters.Length - 1}"));
                syntaxes = restID;
                // TODO: hm. Are we going to have to do something more with scopeSet and scope level to make this work?
                // the compiler is going to need to figure out that these are args
                // I think the scope level should always be 1, because the method is being imported as a top-level var
                // potentially this will not ALWAYS hold and we may need more complex logic
                rest = new Jig.Expansion.Parameter(restID.Symbol, [], 1, parameters.Length - 1 + offset, null);
                lastIndex--;
            }
            System.Collections.Generic.List<Jig.Expansion.Parameter> required = [];
            for (; lastIndex >= 0; lastIndex--) {
                // working from end of parameters[], create Identifiers from parameterinfos and cons them onto the list
                Identifier p = new Identifier(new Symbol(parameters[lastIndex].Name ?? $"arg{lastIndex}"));
                var parameter = new Jig.Expansion.Parameter(p.Symbol, [], 1, lastIndex + offset, null);
                syntaxes = (SchemeValue)Pair.Cons(parameter, syntaxes);
                required.Add(parameter);
            }
            Syntax stx = new Syntax(syntaxes);
            required.Reverse();
            // TODO: should this method be a static method on LambdaParameters?
            return new ParsedLambda.LambdaParameters(stx, required.ToArray(), rest);

        }
        return new ParsedLambda.LambdaParameters(new Syntax(SyntaxList.Null), [], null);
    }

}

public delegate Jig.SchemeValue ClrPrimitiveUnsafe(Jig.List args);

public class ClrPrimitive : SchemeValue, ICallable {
    public ClrPrimitive(MethodInfo mi, TypeResolver tr) {
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
    }

    public ClrPrimitive(ConstructorInfo ctor, TypeResolver tr) {
        Delegate = MakeDelegateForCtor(ctor, tr);
    }
    private ClrPrimitiveUnsafe? MakeDelegateForCtor(ConstructorInfo ctor, TypeResolver tr) {
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
        
        Jig.Types.TypeDescriptor desc = tr.Resolve(mi.DeclaringType);
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