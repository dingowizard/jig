using System.Diagnostics;
using System.Linq.Expressions;
using System.Net;
using System.Runtime.CompilerServices;
using System.Text;
using Jig;
using Microsoft.Scripting.Utils;
using Sys = System.Collections.Generic;

namespace VM;

public class Compiler {


    public Template CompileExprForREPL(
        ParsedExpr x,
        CompileTimeEnvironment ctEnv,
        int scopeLevel = 0,
        int startLine = 0,
        bool tail = false) {

        Sys.List<Jig.Form> literals = [];
        Sys.List<Binding> bindings = [];

        ulong[] code = Compile(x, ctEnv, literals, bindings, scopeLevel, startLine, true); 
        var result = new Template(0, code, bindings.ToArray(), literals.ToArray(), 0, false); // TODO: maybe there should be a different kind of template for this, since we don't need parameters
        // Array.ForEach(Dissassembler.Disassemble(result), Console.WriteLine);
        return result;

    }

    private ulong[] Compile(
        ParsedExpr x,
        CompileTimeEnvironment ctEnv,
        Sys.List<Jig.Form> literals,
        Sys.List<Binding> bindings,
        int scopeLevel,
        int startLine = 0,
        bool tail = false) {
        switch (x) {
            case ParsedLiteral lit:
                return CompileLit(lit, literals, tail);
            case ParsedVariable.TopLevel top:
                return CompileTop(top, ctEnv, bindings, tail);
            case ParsedVariable.Lexical lexVar:
                return CompileLexVar(lexVar, ctEnv, scopeLevel, tail);
            case ParsedIf ifExpr:
                return CompileIfExpr(ifExpr, ctEnv, literals, bindings, scopeLevel, startLine, tail);
            case ParsedLambda le:
                return CompileLambdaExpr(le, ctEnv, literals,  scopeLevel, 0, tail);
            case ParsedList app:
                return CompileApplication(app, ctEnv, literals, bindings, scopeLevel, startLine, tail);
            case ParsedDefine define:
                return CompileDefinition(define, ctEnv, literals, bindings, scopeLevel, startLine, tail);
            case ParsedSet set:
                return CompileSet(set, ctEnv, literals, bindings, scopeLevel, startLine, tail);
            default:
                throw new NotImplementedException($"{x.Print()} of type {x.GetType()} is not supported yet");
        }
    }

    private ulong[] CompileSet(
        ParsedSet setForm,
        CompileTimeEnvironment ctEnv,
        Sys.List<Jig.Form> literals,
        Sys.List<Binding> bindings,
        int scopeLevel,
        int startLine = 0,
        bool tail = false)
    {
        Sys.List<ulong> result = new();
        if (setForm.Variable is ParsedVariable.TopLevel topVar) {
            var bing = ctEnv.LookUpTopLevel(topVar.Identifier.Symbol);
            if (!bindings.Contains(bing)) {
                bindings.Add(bing);
            }
            ulong code = (ulong)OpCode.Store << 56;
            int index = bindings.IndexOf(bing);
            code += (ulong)index;
            result.Add(code);
        } else {
            var lexVar = (ParsedVariable.Lexical)setForm.Variable;
            ulong code = (ulong)OpCode.SetLex << 56;
            int depth = scopeLevel - lexVar.Binding.ScopeLevel;
            code += ((ulong)depth) << 32; // TODO: this could be too big I suppose.
            code += (ulong)lexVar.Binding.VarIndex;
            result.Add(code);
        }

        // We have to compile the lambda function after the variable,
        // because there might be a recursive call to a toplevel
        result.InsertRange(0, Compile(setForm.Value, ctEnv, literals, bindings, scopeLevel, startLine, false));
        if (tail) {
            result.Add((ulong)OpCode.PopContinuation << 56);
        }
        return result.ToArray();
    }
    
    private ulong[] CompileDefinition(
        ParsedDefine defForm,
        CompileTimeEnvironment ctEnv,
        Sys.List<Jig.Form> literals,
        Sys.List<Binding> bindings,
        int scopeLevel,
        int startLine = 0,
        bool tail = false)
    {
        Sys.List<ulong> result = new();
        if (defForm.Variable is ParsedVariable.TopLevel topVar) {
            var bing = ctEnv.DefineTopLevel(topVar.Identifier.Symbol);
            if (!bindings.Contains(bing)) {
                bindings.Add(bing);
            }
            ulong code = (ulong)OpCode.Store << 56;
            int index = bindings.IndexOf(bing);
            code += (ulong)index;
            result.Add(code);
        } else {
            var lexVar = (ParsedVariable.Lexical)defForm.Variable;  
            ulong code = (ulong)OpCode.DefLocal << 56;
            int index = lexVar.Binding.VarIndex;
            code += (ulong)index;
            result.Add(code);
        }

        // We have to compile the lambda function after the variable,
        // because there might be a recursive call to a toplevel
        result.InsertRange(0, Compile(defForm.Value, ctEnv, literals, bindings, scopeLevel, startLine, false));
        if (tail) {
            result.Add((ulong)OpCode.PopContinuation << 56);
        }
        return result.ToArray();
        

    }

    private ulong[] CompileIfExpr(
        ParsedIf ifExpr,
        CompileTimeEnvironment ctEnv,
        Sys.List<Form> literals,
        Sys.List<Binding> bindings,
        int scopeLevel,
        int startLine,
        bool tail)
    {
        int lineNo = startLine;
        var condCodes = Compile(ifExpr.Condition, ctEnv, literals, bindings, scopeLevel, startLine, false);
        lineNo += condCodes.Length;
        lineNo++; // to account for JumpIfFalse instruction
        var thenCodes = Compile(ifExpr.Then, ctEnv, literals, bindings, scopeLevel, lineNo, tail); 
        lineNo += thenCodes.Length;
        lineNo++; // for unconditional jump to end
        // this is the start of the else code so JumpIfFalse should go here
        ulong jumpIfFalse = ((ulong)OpCode.JumpIfFalse << 56) + (ulong)lineNo;
        ulong[] elseCodes = [];
        if (ifExpr.Else is not null) {
            elseCodes = Compile(ifExpr.Else, ctEnv, literals, bindings, scopeLevel, lineNo, tail);
        } else {
            literals.Add(Form.Void);
            int index = literals.IndexOf(Form.Void);
            elseCodes = [((ulong)OpCode.Lit << 56) + (ulong)index];
            if (tail) {
                elseCodes = elseCodes.Append((ulong)OpCode.PopContinuation << 56).ToArray();
            }
        }
        lineNo += elseCodes.Length;
        ulong jump = ((ulong)OpCode.Jump << 56) + (ulong)lineNo;
        return condCodes.Append(jumpIfFalse).Concat(thenCodes).Append(jump).Concat(elseCodes).ToArray();
    }

    public Template Compile(
        ParsedExpr[] sequence,
        CompileTimeEnvironment ctEnv,
        Sys.List<Jig.Form> literals,
        Sys.List<Binding> bindings,
        int scopeLevel,
        int startLine = 0,
        bool tail = false)
    {
        
        Sys.List<ulong> instructions = [];

        int lineNo = startLine;
        foreach (var x in sequence.Take(sequence.Length - 1)) {
            instructions = instructions.Concat(Compile(x, ctEnv, literals, bindings, scopeLevel, lineNo, false)).ToList();
            lineNo += instructions.Count();
        }
        instructions = instructions.Concat(Compile(sequence[sequence.Length - 1], ctEnv, literals, bindings, scopeLevel, lineNo, true)).ToList();
        return new Template(0, instructions.ToArray(), bindings.ToArray(), literals.ToArray(), 0, false);
    }

    public ulong[] CompileLit(
        ParsedLiteral literal,
        Sys.List<Jig.Form> literals,
        bool tail = false)
    {
        var constExpr = (Form)Syntax.ToDatum(literal.Quoted);
        if (!literals.Contains(constExpr)) {
           literals.Add(constExpr); 
        }
        int index = literals.IndexOf(constExpr);
        ulong code = (ulong)OpCode.Lit << 56;
        code += (ulong)index;
        if (tail) {
            return [code, (ulong)OpCode.PopContinuation << 56];
        }
        return [code];
    }

    public ulong[] CompileTop(
        ParsedVariable.TopLevel var,
        CompileTimeEnvironment ctEnv,
        Sys.List<Binding> bindings,
        bool tail = false)
    {
        
        Form.Symbol sym = var.Identifier.Symbol;
        if (!bindings.Any(b => Equals(b.Symbol, sym))) {
            bindings.Add(ctEnv.LookUpTopLevel(sym));
        }
        int index = bindings.FindIndex(b => Equals(b.Symbol, sym));
        ulong code = (ulong)OpCode.Load << 56;
        code += (ulong)index;
        if (tail) {
            return [code, (ulong)OpCode.PopContinuation << 56];
        }
        return [code];

    }

    public ulong[] CompileLexVar(
        ParsedVariable.Lexical var,
        CompileTimeEnvironment ctEnv, // TODO: should scope level be part of ct-env?
        int scopeLevel,
        bool tail = false) {

        // TODO: we already found the bindings when we parsed/expanded
        ulong code = (ulong)OpCode.Local << 56;
        int depth = scopeLevel - var.Binding.ScopeLevel;
        code += ((ulong)depth) << 32; // TODO: this could be too big I suppose.
        
        code += (ulong)var.Binding.VarIndex;
        if (tail) {
            return [code, (ulong)OpCode.PopContinuation << 56];
        }
        return [code];
    }

    public ulong[] CompileLambdaExpr(
        ParsedLambda lambdaExpr,
        CompileTimeEnvironment ctEnv,
        Sys.List<Form> literals,
        int scopeLevel,
        int startLine = 0,
        bool tail = false) {

        Template template = CompileLambdaTemplate(lambdaExpr, ctEnv, scopeLevel);
        literals.Add(template);
        int templateIndex = literals.IndexOf(template);
        Sys.List<ulong> result = [
            (ulong)OpCode.Env << 56, // ENVT
            (ulong)OpCode.Push << 56, // PUSH
            ((ulong)OpCode.Lit << 56) + (ulong)templateIndex, // LIT
            (ulong)OpCode.Push << 56, // PUSH
            (ulong)OpCode.Lambda << 56, // CLOS
        ];
        if (tail) {
            return result.Append((ulong)OpCode.PopContinuation << 56).ToArray();
        }
        return result.ToArray();

    }

    private Template CompileLambdaTemplate(
        ParsedLambda lambdaExpr,
        CompileTimeEnvironment ctEnv,
        int scopeLevel)
    {
        var bindings = new Sys.List<VM.Binding>();
        Sys.List<ulong> codes = [];
        foreach (ParsedVariable.Lexical var in lambdaExpr.Parameters.Required) {
            // TODO: get rid of all of this 
            var binding = new VM.Binding(var.Binding);
            bindings.Add(binding);
            if (var.Binding.Index >= ctEnv.LexVars.Length) {
                Console.WriteLine($"in CompileLambdaTemplate: compiling parameter: {var.Identifier.Symbol.Print()} in {Syntax.ToDatum(lambdaExpr).Print()}");
                Console.WriteLine($"\tLexVars has length {ctEnv.LexVars.Length} but index is {var.Binding.Index}");
            }
            ctEnv.LexVars[var.Binding.Index] = binding;
        }
        
        // TODO: should apply be responsible for this?
        var bindCode = (ulong)OpCode.Bind << 56;
        bindCode += (ulong)lambdaExpr.Parameters.Required.Length;
        codes.Add(bindCode);

        if (lambdaExpr.Parameters.HasRest) {
            var binding = new VM.Binding(lambdaExpr.Parameters.Rest.Binding);
            bindings.Add(binding);
            ctEnv.LexVars[lambdaExpr.Parameters.Rest.Binding.Index] = binding;
            var bindRest = (ulong)OpCode.BindRest << 56;
            bindRest += (ulong)lambdaExpr.Parameters.Required.Length;
            codes.Add(bindRest);
        }

        // TODO: likewise body of parsedlambda should be a collection of parsedexpr
        var body = Compile(
            lambdaExpr.Bodies.Cast<ParsedExpr>().ToArray(),
            ctEnv,
            new Sys.List<Form>(),
            bindings,
            ++scopeLevel,
            codes.Count());

        var result = new Template(lambdaExpr.ScopeVarsCount, codes.Concat(body.Code).ToArray(), body.Bindings, body.Slots, lambdaExpr.Parameters.Required.Length, lambdaExpr.Parameters.HasRest);
        // Console.WriteLine($"***** {Syntax.ToDatum(lambdaExpr).Print()} compiled to: *****");
        // Array.ForEach(Dissassembler.Disassemble(result), Console.WriteLine);
        return result;
        
    }

    public ulong[] CompileApplication(
        ParsedList app,
        CompileTimeEnvironment ctEnv,
        Sys.List<Form> literals,
        Sys.List<Binding> bindings,
        int scopeLevel,
        int startLine = 0,
        bool tail = false)
    {
        Sys.List<ulong> instructions = [];
        var xs = app.ParsedExprs.ToArray();
        // we're going to wait til the end for the push continuation instr because we need to know the address
        // we'll append it to the front
        int lineNo = startLine;
        // eval and push for all args to the call
        for (int i = xs.Length - 1; i > 0; i--) {
            var codes = Compile(xs[i], ctEnv, literals, bindings, scopeLevel, tail ? lineNo : lineNo + 1, false).ToList();
            codes.Add((ulong)OpCode.Push << 56);
            instructions = instructions.Concat(codes).ToList();
            lineNo += codes.Count();
        }

        var codeForProc = Compile(xs[0], ctEnv, literals, bindings, scopeLevel, lineNo, false);
        // don't push it. Call assumes the procedure is in VAL
        instructions.AddRange(codeForProc);
        lineNo += codeForProc.Length;
        
        instructions.Add((ulong)OpCode.Call << 56);
        lineNo++;
        
        
        if (!tail) {
            // the call is not a tail call, so we do want to save a continuation before we start
            var pushContInstruction = (ulong)OpCode.PushContinuation << 56;
            pushContInstruction += (ulong)lineNo + 1;
            instructions.Insert(0, pushContInstruction);
        }

        return instructions.ToArray();
    }
}