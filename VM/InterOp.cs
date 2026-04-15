using System.Diagnostics;
using System.Reflection;
using Jig;
using Jig.Expansion;
using Jig.Types;
using String = Jig.String;
namespace VM;

public class InterOp : IInterOp {
    
    
    // TODO: error messages for argument type errors are not very good.
    // for example, will give failed to find overload when there is only one overload
    // names of constructors should be "Type.new" in errors rather than ".ctor"
    // probably other issues

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
        if (t != null) {
            return [t];
        }

        // Search all loaded assemblies for type with name
        foreach (var asm in AppDomain.CurrentDomain.GetAssemblies()) {
            Type? st = asm.GetType(name);
            if (st != null) {
                t = st;
                break;
            }
        }
        if (t != null) {
            return [t];
        }
        // otherwise treat as namespace
        return AppDomain.CurrentDomain
            .GetAssemblies()
            .SelectMany(a => a.GetTypes())
            .Where(t => t.Namespace == name
                        || t.Namespace?.StartsWith(name + ".") == true);
    }

    public Binding[] BindingsFromType(Type type) {
        
        System.Collections.Generic.List<Binding> bindings = [];
        int indexForBinding = 0;
        
        
        if (type.IsEnum) {
            var enumBinding = new Binding(new Jig.Expansion.Parameter(new Symbol(type.Name), [], 0, indexForBinding++, null),
                new Location(new LiteralExpr<Type>(type)));
            bindings.Add(enumBinding);
            foreach (var enumVal in Enum.GetValues(type)) {
                string name = type.Name + "." + Enum.GetName(type, enumVal);
                int enumInt = (int)enumVal;
                var bg = new Binding(new Jig.Expansion.Parameter(new Symbol(name), [], 0, indexForBinding++, null),
                    new Location(new Integer(enumInt)));
                bindings.Add(bg);
            }
            return bindings.ToArray(); // TODO: we're sure enum type can't have any of the below?
        }
        
        var typeBinding = new Binding(new Jig.Expansion.Parameter(new Symbol(type.FullName), [], 0, indexForBinding++, null),
            new Location(new LiteralExpr<Type>(type)));
        bindings.Add(typeBinding);
        
        var constantFields =
            type.GetFields(BindingFlags.Public | BindingFlags.Static)
                .Where(fi => (fi.IsLiteral || fi.IsInitOnly) && CanHandleType(fi.FieldType));
        foreach (var f in constantFields) {
            SchemeValue schemeValue = TypeResolver.Resolve(f.FieldType).WrapReturn(f.IsLiteral ? f.GetRawConstantValue() : f.GetValue(null));

            string fullName =  f.ReflectedType.Name +  "." +   f.Name;
            // Console.WriteLine($"field = {fullName}");
            var bg = new Binding(new Jig.Expansion.Parameter(new Symbol(fullName), [], 0, indexForBinding++, null),
                new Location(schemeValue));
            bindings.Add(bg);
        }
        
        var staticMethodInfos =
            type.GetMethods(BindingFlags.Public | BindingFlags.Static)
                .Where(mi => CanHandleType(mi.ReturnType) && mi.GetParameters().All(p => CanHandleType(p.ParameterType)))
                .Where(mi => !mi.GetParameters().Any(p => p.IsDefined(typeof(ParamArrayAttribute), inherit: false)))
                .Where(mi => !mi.IsSpecialName)
                .GroupBy(mi => mi.Name);
        foreach (var methodGroup in staticMethodInfos) {
            var arr = methodGroup.ToArray();
            string fullName = arr[0].ReflectedType.Name + "."  +  arr[0].Name; 
            // Console.WriteLine($"trying to import {fullName}");
            SchemeValue clrProcedure = Bool.False;
            try {
                clrProcedure = ProcedureFromMethodGroup(arr);
            } catch (Exception x) {
                Console.WriteLine($"couldn't import {methodGroup.ElementAt(0).Name}. {x.Message}");
                continue;
            }
            var bg = new Binding(
                new Jig.Expansion.Parameter(new Symbol(fullName), [], 0, indexForBinding++, null),
                new Location(clrProcedure));
            // Console.WriteLine($"adding {fullName}");
            bindings.Add(bg);
        }
        
        // TODO: we can get rid of these type filters when the type resolver knows what to do with generics and interfaces
        var instanceMethodGroups =
            type.GetMethods(BindingFlags.Public | BindingFlags.Instance)
                .Where(mi => CanHandleType(mi.ReflectedType) &&
                             CanHandleType(mi.ReturnType) &&
                             mi.GetParameters().All(p => CanHandleType(p.ParameterType)))
                .Where(mi => !mi.GetParameters().Any(p => p.IsDefined(typeof(ParamArrayAttribute), inherit: false)))
                .Where(mi => !mi.IsSpecialName)
                .GroupBy(mi => mi.Name);
        foreach (var methodGroup in instanceMethodGroups) {
            var arr = methodGroup.ToArray();
            string fullName = arr[0].ReflectedType.Name + "."  +  arr[0].Name; 
            Procedure clrProcedure = ProcedureFromMethodGroup(arr);
            var bg = new Binding(
                new Jig.Expansion.Parameter(new Symbol(fullName), [], 0, indexForBinding++, null),
                new Location(clrProcedure));
            bindings.Add(bg);
            
        }
        var instanceProperties =
            type.GetProperties(BindingFlags.Public | BindingFlags.Instance)
                .Where(p => CanHandleType(p.PropertyType));
        foreach (var prop in instanceProperties) {
            var propertyName = prop.ReflectedType.Name + "." + prop.Name;
            var procedure = ProcedureFromInstanceProperty(prop);
            var bg = new Binding(
                new Jig.Expansion.Parameter(new Symbol(propertyName), [], 0, indexForBinding++, null),
                new Location(procedure));
            bindings.Add(bg);
        }
        var staticProperties =
            type.GetProperties(BindingFlags.Public | BindingFlags.Static)
                .Where(p => CanHandleType(p.PropertyType));
        foreach (var prop in staticProperties) {
            if (prop.GetGetMethod() != null) {
                var propertyName = prop.ReflectedType.Name + "." + prop.Name;
                var procedure = ProcedureForGetter(prop);
                var bg = new Binding(
                    new Jig.Expansion.Parameter(new Symbol(propertyName), [], 0, indexForBinding++, null),
                    new Location(procedure));
                bindings.Add(bg);
                
            }
            
        }
        var ctors =
            type.GetConstructors(BindingFlags.Public | BindingFlags.Instance)
                .Where(ci => CanHandleType(ci.ReflectedType))
                .Where(ci => ci.GetParameters().All(p => CanHandleType(p.ParameterType)))
                .ToArray();
        if (ctors.Length != 0) {
            var ctorProc = ProcedureFromMethodGroup(ctors);
            var bindingForCtor = new Binding(
                new Jig.Expansion.Parameter(new Symbol(ctors[0].ReflectedType.Name + ".new"), [], 0, indexForBinding++, null),
                new Location(ctorProc));
            bindings.Add(bindingForCtor);
        }
        return bindings.ToArray();
    }

    private bool CanHandleType(Type type, bool allowOthers = false) {

        if (type.IsConstructedGenericType) {
            return false;
        }
        if (_bannedTypes.Contains(type)) {
            return false;
        } 
        if (_typesWeKnow.Contains(type)) {
            return true;
        }
        return allowOthers;

    }

    private static readonly Type[] _bannedTypes = [];

    private static readonly Type[] _typesWeKnow = [
        typeof(SchemeValue),
        typeof(SchemeValue[]),
        typeof(String),
        typeof(Integer),
        typeof(Symbol),
        typeof(Vector),
        typeof(Bool),
        typeof(Jig.Char),
        typeof(int),
        typeof(double),
        typeof(string),
        typeof(string[]),
        typeof(bool),
        typeof(char),
        typeof(char[])];
    public Library LibraryFromTypes(IEnumerable<Type> ts) {


        System.Collections.Generic.List<Binding> bindings = [];
        foreach (var t in ts) {
            bindings.AddRange(BindingsFromType(t));
        }
        return new Library(bindings, []);
    }

    public ILibrary LibraryFromType(Type type) {
        return new Library(BindingsFromType(type), []);
    }

    public ILibrary ImportClrNameSpace(string name) {
        var ts = ResolveClrName(name);
        return LibraryFromTypes(ts);

    }

    private string MethodName(MethodInfo mi) {
        
        System.Collections.Generic.List<string> paramNameAndTypes = [];
        foreach (var p in mi.GetParameters()) {
            paramNameAndTypes.Add(p.ParameterType.Name);
        }
        string ps = string.Join("->", paramNameAndTypes);
        string returnType = mi.ReturnType.Name;
        return mi.ReflectedType.Name +  "."  + mi.Name + "/" + ps + (ps != "" ? "->" + returnType : returnType);
    }

    private Procedure MakeProcedure(ParsedLambda.LambdaParameters lambdaParams, ParsedForm[] body, string name) {
        // TODO: we create a ParsedLambda and then send it to the compiler.
        // but would it make more sense to build a Datum and send that to the Expander?
        // One thing to consider is that we create literal ClrPrimitives
        // and out of those ParsedLiterals as operators for the applications
        // But I suppose we could just as well make datum literals like integers
        // It's just that these are not readable things so it would be odd for them to be datums
        
        var parsedLambda = new ParsedLambda(
            new Identifier(new Symbol("lambda")),
            lambdaParams,
            lambdaParams.Required.Length + (lambdaParams.HasRest ? 1 : 0),
            body);
        // Console.WriteLine($"{Syntax.ToDatum(parsedLambda).Print()}");
        var compiler = new Compiler();
        
        var template = compiler.CompileLambdaTemplate(parsedLambda, Evaluator.Environment, 0);
        template.Name = new Identifier(new Symbol(name));
        return new Procedure(Evaluator.Environment, template);
        
    }
    
    public Procedure ProcedureFromMethodGroup(MethodBase[] methodBases) {
        
        if (HaveSameNumberParameters(methodBases)) {
            
            ParsedLambda.LambdaParameters lambdaParameters = LambdaParametersForMethodGroupSameParamNum(methodBases);
            System.Collections.Generic.List<(ParsedForm @if, ParsedApplication call)> stmts = [];
            foreach (var m in methodBases) {
                stmts.Add( ConditionAndCallForOverride(m, lambdaParameters));
            }
            return MakeProcedure(lambdaParameters, BodyForMethodGroup(methodBases, stmts), methodBases[0].Name);
        }
        return ProcedureFromMethodsVariousParamLengths(methodBases);
    }
    
    private Procedure ProcedureFromMethodsVariousParamLengths(MethodBase[] methodBases) {
        // TODO: need to deal with methods that have a params parameter at end
        var sorted = methodBases.OrderBy(mi => mi.GetParameters().Length);
        var firstMethodBase = sorted.ElementAt(0);
        var numRequired = firstMethodBase.GetParameters().Length;
        if (firstMethodBase is MethodInfo mi && !mi.IsStatic) {
            numRequired += 1;
        }
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
        var lambdaParams = new ParsedLambda.LambdaParameters(parameters.ToArray(), rest);
        return MakeProcedure(lambdaParams, BodyForMethodGroupVariousParamLengths(numRequired, lambdaParams, sorted), methodBases[0].Name);
    }
    
    private ParsedForm[] BodyForMethodGroupVariousParamLengths(int shortest, ParsedLambda.LambdaParameters lambdaParams, IOrderedEnumerable<MethodBase> methodBases) {
        var mi = methodBases.ElementAt(0);

        // Console.WriteLine($"Making procedure for {mi.Name}. The override with the least parameters has {shortest} parameters.");
        // Console.WriteLine($"\tthe lambda parameters for the function are: {Syntax.E(lambdaParams).Print()}. required = {lambdaParams.Required.Length}");
        return [BodyExprMethodGroupVariousLengths(shortest, lambdaParams, methodBases, mi.Name, mi.ReflectedType)];
    }
    
    private ParsedForm BodyExprMethodGroupVariousLengths(int shortest, ParsedLambda.LambdaParameters lambdaParams, IOrderedEnumerable<MethodBase> methodBases, string name, Type? miDeclaringType) {
        if (methodBases.Count() == 0) {
            return CouldNotResolveErrorCall(name, miDeclaringType);
        }
        int n = methodBases.ElementAt(0).GetParameters().Length;
        System.Collections.Generic.List<MethodBase> shortestMethods = [];
        System.Collections.Generic.List<MethodBase> rest = [];
        foreach (var mb in methodBases) {
            if (mb.GetParameters().Length  == n) {
                shortestMethods.Add(mb);
            } else {
                rest.Add(mb);
            }
        }
        if (methodBases.ElementAt(0) is MethodInfo met && !met.IsStatic) {
            n += 1;
        }
        var cond = ConditionForMethodGroupParamLength(shortest, n, lambdaParams.Rest);
        var then = ThenBranchForMethodGroupVariousLengths(lambdaParams, shortestMethods, lambdaParams);
        var @else = BodyExprMethodGroupVariousLengths(shortest, lambdaParams, rest.OrderBy(mi => mi.GetParameters().Length), name, miDeclaringType); 
        return new ParsedIf(
            new Identifier(new Symbol("if")),
            cond,
            then,
            @else);
    }
    
    private ParsedForm ThenBranchForMethodGroupVariousLengths(
        ParsedLambda.LambdaParameters lambdaParams,
        System.Collections.Generic.List<MethodBase> shortestMethods,
        ParsedLambda.LambdaParameters lambdaParameters) {
        // this has to make something like:
        // ((lambda (x0 x1 x2 x3) ...)
        //  x0 x1 (car xs) (car (cdr xs)))
        // assuming there were two required args and a rest parameter
        // we should be able to reuse some code from where we made a procedure for method groups of same parameter length

        int n = 0;
        if (shortestMethods[0] is MethodInfo mi && !mi.IsStatic) {
            n = 1;
        }
        int numRestArgs = (shortestMethods[0].GetParameters().Count() + n) - lambdaParameters.Required.Length;
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

    private ParsedForm[] BodyForMethodGroup(MethodBase[] methodBases, System.Collections.Generic.List<(ParsedForm @if, ParsedApplication call)> stmts) {
        return [NestedIfsForMethodGroup(methodBases, stmts)];
    }
    
    private ParsedForm NestedIfsForMethodGroup(MethodBase[] methodBases, IEnumerable<(ParsedForm @if, ParsedApplication call)> stmts) {
        if (stmts.Count() == 1) {
            return new ParsedIf(
                new Identifier(new Symbol("if")),
                stmts.ElementAt(0).@if,
                stmts.ElementAt(0).call,
                CouldNotResolveErrorCall(methodBases[0].Name, methodBases[0].ReflectedType));
        }
        return new ParsedIf(
            new Identifier(new Symbol("if")),
            stmts.ElementAt(0).@if,
            stmts.ElementAt(0).call,
            NestedIfsForMethodGroup(methodBases, stmts.Skip(1)));
    }
    
    private (ParsedForm test, ParsedApplication call) ConditionAndCallForOverride(MethodBase methodBase, ParsedLambda.LambdaParameters lambdaParameters) {
        ParsedForm condition = ConditionForOverride(methodBase, methodBase.GetParameters().ToList(), lambdaParameters.Required.ToList());
        ParsedApplication app = CallForOverride(methodBase, lambdaParameters);
        return (condition, app);

    }
    
    private ParsedForm ConditionForOverride(MethodBase methodBase, System.Collections.Generic.List<ParameterInfo> toList, System.Collections.Generic.List<Parameter> parameters) {
        if (methodBase is MethodInfo mi && !mi.IsStatic) {
            return new ParsedIf(
                new Syntax(new Symbol("if")),
                PredicateApplicationFromType(methodBase.ReflectedType, new ParsedVariable.Lexical(new Identifier(parameters.ElementAt(0).Symbol), parameters.ElementAt(0), null)),
                ConditionForOverrideAux(toList, parameters.Skip(1)),
                new ParsedLiteral(new Syntax(new Symbol("quote")), new Syntax.Literal(Bool.False)));
        }
        return ConditionForOverrideAux(toList, parameters);
    }
    
    private ParsedForm ConditionForOverrideAux(IEnumerable<ParameterInfo> parameterInfos, IEnumerable<Parameter> parameters) {
        
        if(parameterInfos.Count() != parameters.Count()) throw new Exception("parameter counts don't match");
        if (parameters.Count() == 0) return new ParsedLiteral(new Syntax(new Symbol("quote")), new Syntax.Literal(Bool.True));
        return new ParsedIf(
            new Syntax(new Symbol("if")),
            PredicateApplicationFromParameterInfo(parameterInfos.ElementAt(0), new ParsedVariable.Lexical(new Identifier(parameters.ElementAt(0).Symbol), parameters.ElementAt(0), null)),
            ConditionForOverrideAux(parameterInfos.Skip(1), parameters.Skip(1)),
            new ParsedLiteral(new Syntax(new Symbol("quote")), new Syntax.Literal(Bool.False)));
    }
    
    
    private ParsedApplication CallForOverride(MethodBase methodBase, ParsedLambda.LambdaParameters lambdaParameters) {
        var clrMethod = new ClrPrimitive(methodBase, TypeResolver);
        ParsedForm[] forms = [new ParsedLiteral(new Identifier(new Symbol("quote")), new Syntax.Literal(clrMethod), null)];
        var args = lambdaParameters.Required.Select(p => new ParsedVariable.Lexical(new Identifier(p.Symbol), p, null));
        return new ParsedApplication(forms.Concat(args), null);
    }
    
    private ParsedLambda.LambdaParameters LambdaParametersForMethodGroupSameParamNum(MethodBase[] methodBasses) {
        Debug.Assert(HaveSameNumberParameters(methodBasses));
        return LambdaParametersForMethod(methodBasses[0]);
    }
    
    
    private ParsedLambda.LambdaParameters LambdaParametersForMethod(MethodBase methodBase) {
        if (methodBase is MethodInfo mi && !mi.IsStatic) {
            var ps = LambdaParametersFromParameterInfos(methodBase.GetParameters(), 1);
            Identifier objParam = new Identifier(new Symbol("arg0"));
            var parameter = new Jig.Expansion.Parameter(objParam.Symbol, [], 1, 0, null);
            return new ParsedLambda.LambdaParameters(
                new[]
                {
                    parameter
                }.Concat(ps.Required).ToArray(),
                ps.Rest);
        } else {
            // ConstructorInfo or static method
            return LambdaParametersFromParameterInfos(methodBase.GetParameters(), 0);
        }
    }
    
    private bool HaveSameNumberParameters(MethodBase[] methodBases) {
        int n = methodBases[0].GetParameters().Length;
        foreach (var m in methodBases.Skip(1)) {
            if (n != m.GetParameters().Length) {
                return false;
            }
        }
        return true;
    }
    
    // TODO: would it make more sense to put all of these on the ProcedureFrom methods on the Procedure class?
    public Procedure ProcedureForGetter(PropertyInfo propertyInfo) {
        
        MethodInfo? getMethod = propertyInfo.GetGetMethod();
        if (getMethod == null) throw new Exception();
        var lambdaParams = LambdaParametersForMethod(getMethod);// NOTE: I think this will handle static and instance
        return MakeProcedure(lambdaParams,  [CallForOverride(getMethod, lambdaParams)], propertyInfo.ReflectedType.Name + "." + propertyInfo.Name);
    }

    public Procedure ProcedureFromInstanceProperty(PropertyInfo prop) {
        var lambdaParameters = LambdaParametersFromInstanceProperty(prop);
        return MakeProcedure(lambdaParameters, LambdaBodyForInstanceMethod(prop.GetGetMethod(), lambdaParameters), prop.ReflectedType.Name + "." + prop.Name);
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
    
    private ParsedForm[] LambdaBodyForInstanceMethod(MethodInfo methodInfo, ParsedLambda.LambdaParameters lambdaParameters) {
        var firstParam = lambdaParameters.Required[0];
        ParsedVariable.Lexical objectArg = new ParsedVariable.Lexical(new Identifier(firstParam.Symbol), firstParam, null);
        System.Collections.Generic.List<ParsedForm> ifs = [IfExprForObjectArg(methodInfo.Name, methodInfo.ReflectedType, objectArg)];
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
        Identifier objParam = new Identifier(new Symbol(methodInfo.ReflectedType.Name.ToLower()));
        var parameter = new Jig.Expansion.Parameter(objParam.Symbol, [], 1, 0, null);
        return new ParsedLambda.LambdaParameters(
            new[]
            {
                parameter
            }.Concat(ps.Required).ToArray(),
            ps.Rest);
    }
    private ParsedLambda.LambdaParameters LambdaParametersFromParameterInfos(ParameterInfo[] parameters, int offset = 0) {
        if (parameters.Length != 0) {

            ParameterInfo last = parameters[^1];
            int lastIndex = parameters.Length - 1;
            Jig.Expansion.Parameter? rest  = null;
            if (last.IsDefined(typeof(ParamArrayAttribute), false)) {
                Identifier restID = new Identifier(new Symbol(last.Name ?? $"arg{parameters.Length - 1}"));
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
                required.Add(parameter);
            }
            required.Reverse();
            // TODO: should this method be a static method on LambdaParameters?
            return new ParsedLambda.LambdaParameters(required.ToArray(), rest);

        }
        return new ParsedLambda.LambdaParameters([], null);
    }

}
