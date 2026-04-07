using System.Diagnostics;
using Jig;
using Jig.Expansion;
using System.Reflection;
using System.Reflection.Metadata;
namespace VM;

public class ReflectionLibrary : ILibrary {


    public ReflectionLibrary(InterOp interOp) {

        
        System.Collections.Generic.List<Binding> varBindings = [];
        int index = 0;
        
        MethodInfo? getTypeMethodInfo = typeof(Type).GetMethod(nameof(Type.GetType), BindingFlags.Static | BindingFlags.Public, [typeof(string)]);
        Debug.Assert(getTypeMethodInfo != null);
        Procedure getTypeProcedure = interOp.ProcedureFromStaticMethodInfo(getTypeMethodInfo);
        varBindings.Add(new Binding(
            new (new Symbol("get-type"), [], 0, index++, null),
            new Location(getTypeProcedure)));
        
        MethodInfo? getMethodMethodInfo = typeof(Type).GetMethod(nameof(Type.GetMethod), BindingFlags.Instance | BindingFlags.Public, [typeof(string), typeof(Type[])]);
        Debug.Assert(getMethodMethodInfo != null);
        Procedure getMethodProcedure = interOp.ProcedureFromInstanceMethodInfo(getMethodMethodInfo);
        varBindings.Add(new Binding(
            new (new Symbol("get-method"), [], 0, index++, null),
            new Location(getMethodProcedure)));
        
        MethodInfo? getProcedureFromMethodMethodInfo = typeof(ReflectionLibrary).GetMethod(nameof(ProcedureFromMethodInfo), BindingFlags.Static | BindingFlags.NonPublic, [typeof(MethodInfo)]);
        Debug.Assert(getProcedureFromMethodMethodInfo != null);
        Procedure getProcedureFromMethodProcedure = interOp.ProcedureFromStaticMethodInfo(getProcedureFromMethodMethodInfo);
        varBindings.Add(new Binding(
            new (new Symbol("procedure<-method"), [], 0, index++, null),
            new Location(getProcedureFromMethodProcedure)));
        
        MethodInfo? mi = typeof(ReflectionLibrary).GetMethod(nameof(ProcedureFromMethodInfo), BindingFlags.NonPublic | BindingFlags.Static, [typeof(string), typeof(string), typeof(string[])]);
        Debug.Assert(mi != null);
        Procedure procedureFromMethodInfo = interOp.ProcedureFromStaticMethodInfo(mi);
        varBindings.Add(new Binding(
            new (new Symbol("procedure<-clr-method"), [], 0, index++, null),
            new Location(procedureFromMethodInfo)));
        
        MethodInfo? propertyMI = typeof(ReflectionLibrary).GetMethod(nameof(ProcedureFromPropertyInfo), BindingFlags.NonPublic | BindingFlags.Static, [typeof(string), typeof(string)]);
        Debug.Assert(propertyMI != null);
        Procedure procedureFromPropertyInfo = interOp.ProcedureFromStaticMethodInfo(propertyMI);
        varBindings.Add(new Binding(
            new (new Symbol("procedure<-clr-property"), [], 0, index++, null),
            new Location(procedureFromPropertyInfo)));
        
        VariableExports = varBindings.ToArray();
        KeywordExports = [];
    }


    private static SchemeValue ProcedureFromMethodInfo(MethodInfo methodInfo) {
        if (methodInfo.IsStatic) {
            // TODO: the reason for this Gloabl InterOp insanity is that we need a static method otherwise we have to pass an InterOp instance in
            return InterOp.GlobalInterOp.ProcedureFromStaticMethodInfo(methodInfo);
        }
        return InterOp.GlobalInterOp.ProcedureFromInstanceMethodInfo(methodInfo);
    }

    private static SchemeValue ProcedureFromMethodInfo(string typeName, string methodName, string[] parameterTypes) {
        Type? type = Type.GetType(typeName); // TODO: better if scheme did this so that we could raise scheme errors
        if (type == null) throw new Exception($"couldn't find type {typeName}");
        System.Collections.Generic.List<Type> ptypes = [];
        foreach (var typeString in parameterTypes) {
            var pType = Type.GetType(typeString);
            if (pType == null) throw new Exception($"couldn't find type {typeString}");
            ptypes.Add(pType);
        }
        MethodInfo? methodInfo = type.GetMethod(methodName, ptypes.ToArray());
        if (methodInfo == null) throw new Exception($"couldn't find method {methodName}");
        return ProcedureFromMethodInfo(methodInfo);

    }
    
    // TODO: indexers. get- and set-
    
    private static SchemeValue ProcedureFromPropertyInfo(string typeName, string methodName) {
        Type? type = Type.GetType(typeName); // TODO: better if scheme did this so that we could raise scheme errors
        if (type == null) throw new Exception($"couldn't find type {typeName}");
        PropertyInfo? propertyInfo = type.GetProperty(methodName);
        if (propertyInfo == null) throw new Exception($"couldn't find method {methodName}");
        // TODO: static properties
        // if (propertyInfo.IsStatic) {
        //     // TODO: the reason for this Gloabl InterOp insanity is that we need a static method otherwise we have to pass an InterOp instance in
        //     return InterOp.GlobalInterOp.ProcedureFromStaticMethodInfo(propertyInfo);
        // }
        return InterOp.GlobalInterOp.ProcedureFromInstanceProperty(propertyInfo);

    }
    public IEnumerable<Binding> VariableExports {get;}
    // TODO: why are these two different?
    // Because keywords have to be bound to expansion rules
    public IEnumerable<(Symbol, IExpansionRule)> KeywordExports {get;}
}