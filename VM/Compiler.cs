using Jig;
using Sys = System.Collections.Generic;

namespace VM;

public class Compiler {

    public Template CompileExprForREPL(ParsedExpr x,
        CompileTimeEnvironment ctEnv,
        int scopeLevel = 0,
        int startLine = 0) {

        Sys.List<Form> literals = [];
        Sys.List<Binding> bindings = [];

        ulong[] code = Compile(x, ctEnv, literals, bindings, Context.Tail, scopeLevel, startLine); 
        var result = new Template(0, code, bindings.ToArray(), literals.ToArray(), 0, false); // TODO: maybe there should be a different kind of template for this, since we don't need parameters
        // Array.ForEach(Disassembler.Disassemble(result), Console.WriteLine);
        return result;

    }

    private ulong[] Compile(ParsedExpr x,
        CompileTimeEnvironment ctEnv,
        Sys.List<Form> literals,
        Sys.List<Binding> bindings,
        Context context,
        int scopeLevel,
        int startLine = 0) {
        switch (x) {
            case ParsedLiteral lit:
                return CompileLit(lit, literals, context);
            case ParsedVariable.TopLevel top:
                return CompileTop(top, ctEnv, bindings, context);
            case ParsedVariable.Lexical lexVar:
                return CompileLexVar(lexVar, ctEnv, context, scopeLevel);
            case ParsedIf ifExpr:
                return CompileIfExpr(ifExpr, ctEnv, literals, bindings, context, scopeLevel, startLine);
            case ParsedLambda le:
                return CompileLambdaExpr(le, ctEnv, literals, context, scopeLevel);
            case ParsedBegin begin:
                return CompileBegin(begin, ctEnv, literals, bindings, context, scopeLevel, startLine);
            case ParsedApplication app:
                return CompileApplication(app, ctEnv, literals, bindings, context, scopeLevel, startLine);
            case ParsedDefine define:
                return CompileDefinition(define, ctEnv, literals, bindings, context, scopeLevel, startLine);
            case ParsedSet set:
                return CompileSet(set, ctEnv, literals, bindings, context, scopeLevel, startLine);
            default:
                throw new NotImplementedException($"{x.Print()} of type {x.GetType()} is not supported yet");
        }
    }

    private ulong[] CompileBegin(ParsedBegin begin, CompileTimeEnvironment ctEnv, Sys.List<Form> literals,
        Sys.List<Binding> bindings,
        Context context, int scopeLevel, int startLine)
    {
        // NOTE: wrong type of begin in wrong context is assumed to have been caught as a syntax error
        var sequence = begin.Forms;
        Sys.List<ulong> instructions = [];

        int lineNo = startLine;
        foreach (var x in sequence.Take(sequence.Length - 1)) {
            instructions = instructions.Concat(Compile(x, ctEnv, literals, bindings, Context.NonTailBody, scopeLevel, lineNo)).ToList();
            lineNo += instructions.Count();
        }
        // add instruction for expr in tail position
        return instructions.Concat(Compile(sequence[sequence.Length - 1], ctEnv, literals, bindings, context, scopeLevel, lineNo)).ToArray();
    }

    private ulong[] CompileSet(ParsedSet setForm,
        CompileTimeEnvironment ctEnv,
        Sys.List<Form> literals,
        Sys.List<Binding> bindings,
        Context context,
        int scopeLevel,
        int startLine = 0)
    {
        Sys.List<ulong> result = new();
        if (setForm.Variable is ParsedVariable.TopLevel topVar) {
            var bing = ctEnv.LookUpTopLevel(topVar.Identifier.Symbol);
            if (!bindings.Contains(bing)) {
                bindings.Add(bing);
            }
            ulong code = (ulong)OpCode.SetTop << 56;
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
        result.InsertRange(0, Compile(setForm.Value, ctEnv, literals, bindings, Context.Argument, scopeLevel, startLine));
        if (context == Context.Tail) {
            result.Add((ulong)OpCode.PopContinuation << 56);
        }
        return result.ToArray();
    }
    
    private ulong[] CompileDefinition(ParsedDefine defForm,
        CompileTimeEnvironment ctEnv,
        Sys.List<Form> literals,
        Sys.List<Binding> bindings,
        Context context,
        int scopeLevel,
        int startLine = 0)
    {
        Sys.List<ulong> result = new();
        if (defForm.Variable is ParsedVariable.TopLevel topVar) {
            var bing = ctEnv.DefineTopLevel(topVar.Identifier.Symbol);
            if (!bindings.Contains(bing)) {
                bindings.Add(bing);
            }
            ulong code = (ulong)OpCode.SetTop << 56;
            int index = bindings.IndexOf(bing);
            code += (ulong)index;
            result.Add(code);
        } else {
            var lexVar = (ParsedVariable.Lexical)defForm.Variable;  
            ulong code = (ulong)OpCode.SetLex << 56;
            int index = lexVar.Binding.VarIndex;
            code += (ulong)index;
            result.Add(code);
        }

        // We have to compile the lambda function after the variable,
        // because there might be a recursive call to a toplevel
        if (defForm.Value is not null) {
            result.InsertRange(0, Compile(defForm.Value, ctEnv, literals, bindings, Context.Argument, scopeLevel, startLine));
        } else {
            // push literal void in cases like "(define a)"
            if (!literals.Contains(Form.Void)) {
                literals.Add(Form.Void); 
            }
            int index = literals.IndexOf(Form.Void);
            ulong lit = (ulong)OpCode.Lit << 56;
            lit += (ulong)index;
            result.InsertRange(0, [lit, (ulong)OpCode.Push << 56]);
        }
        if (context == Context.Tail) {
            result.Add((ulong)OpCode.PopContinuation << 56);
        }
        return result.ToArray();
    }

    private ulong[] CompileIfExpr(ParsedIf ifExpr,
        CompileTimeEnvironment ctEnv,
        Sys.List<Form> literals,
        Sys.List<Binding> bindings,
        Context context,
        int scopeLevel,
        int startLine)
    {
        int lineNo = startLine;
        var condCodes = Compile(ifExpr.Condition, ctEnv, literals, bindings, Context.Argument, scopeLevel, startLine);
        lineNo += condCodes.Length;
        lineNo++; // to account for JumpIfFalse instruction
        var thenCodes = Compile(ifExpr.Then, ctEnv, literals, bindings, context, scopeLevel, lineNo); 
        lineNo += thenCodes.Length;
        lineNo++; // for unconditional jump to end
        // this is the start of the else code so JumpIfFalse should go here
        ulong jumpIfFalse = ((ulong)OpCode.JumpIfFalse << 56) + (ulong)lineNo;
        var elseCodes = ifExpr.Else is not null
            ? Compile(ifExpr.Else, ctEnv, literals, bindings, context, scopeLevel, lineNo)
            : CompileLit(ParsedLiteral.Void, literals, context);
        lineNo += elseCodes.Length;
        ulong jump = ((ulong)OpCode.Jump << 56) + (ulong)lineNo;
        return condCodes.Append(jumpIfFalse).Concat(thenCodes).Append(jump).Concat(elseCodes).ToArray();
    }

    public Template CompileSequence(ParsedExpr[] sequence,
        CompileTimeEnvironment ctEnv,
        Sys.List<Form> literals,
        Sys.List<Binding> bindings,
        int scopeLevel,
        int startLine = 0)
    {
        
        Sys.List<ulong> instructions = [];

        int lineNo = startLine;
        foreach (var x in sequence.Take(sequence.Length - 1)) {
            instructions = instructions.Concat(Compile(x, ctEnv, literals, bindings, Context.NonTailBody, scopeLevel, lineNo)).ToList();
            lineNo += instructions.Count();
        }
        // add instruction for expr in tail position
        instructions = instructions.Concat(Compile(sequence[sequence.Length - 1], ctEnv, literals, bindings, Context.Tail, scopeLevel, lineNo)).ToList();
        return new Template(0, instructions.ToArray(), bindings.ToArray(), literals.ToArray(), 0, false);
    }

    public ulong[] CompileLit(
        ParsedLiteral literal,
        Sys.List<Form> literals,
        Context context)
    {
        var constExpr = (Form)Syntax.ToDatum(literal.Quoted);
        if (!literals.Contains(constExpr)) {
           literals.Add(constExpr); 
        }
        int index = literals.IndexOf(constExpr);
        ulong lit = (ulong)OpCode.Lit << 56;
        lit += (ulong)index;
        return CodeForContext(lit, context);
    }

    public ulong[] CompileTop(
        ParsedVariable.TopLevel var,
        CompileTimeEnvironment ctEnv,
        Sys.List<Binding> bindings,
        Context context)
    {
        
        Form.Symbol sym = var.Identifier.Symbol;
        if (!bindings.Any(b => Equals(b.Symbol, sym))) {
            bindings.Add(ctEnv.LookUpTopLevel(sym));
        }
        int index = bindings.FindIndex(b => Equals(b.Symbol, sym));
        ulong code = (ulong)OpCode.Top << 56;
        code += (ulong)index;
        return CodeForContext(code, context);

    }

    private static ulong[] CodeForContext(ulong code, Context context)
    {
        if (context == Context.Tail) {
            return [code, (ulong)OpCode.Push << 56, (ulong)OpCode.PopContinuation << 56];
        }

        if (context == Context.Argument) {
            return [code, (ulong)OpCode.Push << 56];
        }
        // NonTailBody. TODO: others? operator?
        return [code];
        
    }

    private static ulong[] CodeForContext(IEnumerable<ulong> codes, Context context)
    {
        if (context == Context.Tail) {
            return [.. codes, (ulong)OpCode.Push << 56, (ulong)OpCode.PopContinuation << 56];
        }

        if (context == Context.Argument) {
            return [.. codes, (ulong)OpCode.Push << 56];
        }
        // TODO: maybe don't emit anything if in a non-tail body
        return codes.ToArray();
        
    }
    public ulong[] CompileLexVar(ParsedVariable.Lexical var,
        CompileTimeEnvironment ctEnv, // TODO: should scope level be part of ct-env?
        Context context,
        int scopeLevel
    ) {

        ulong code = (ulong)OpCode.Lex << 56;
        int depth = scopeLevel - var.Binding.ScopeLevel;
        code += ((ulong)depth) << 32; // TODO: this could be too big I suppose.
        
        code += (ulong)var.Binding.VarIndex;
        return CodeForContext(code, context);
    }

    public ulong[] CompileLambdaExpr(ParsedLambda lambdaExpr,
        CompileTimeEnvironment ctEnv,
        Sys.List<Form> literals,
        Context context,
        int scopeLevel,
        int startLine = 0) {

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

        return CodeForContext(result, context);
        
    }

    private Template CompileLambdaTemplate(
        ParsedLambda lambdaExpr,
        CompileTimeEnvironment ctEnv,
        int scopeLevel)
    {
        var bindings = new Sys.List<Binding>();
        Sys.List<ulong> codes = [];
        
        // TODO: should apply be responsible for this?
        if (lambdaExpr.Parameters.Required.Length != 0) {
            var bindCode = (ulong)OpCode.Bind << 56;
            bindCode += (ulong)lambdaExpr.Parameters.Required.Length;
            codes.Add(bindCode);
        }
        if (lambdaExpr.Parameters.HasRest) {
            var bindRest = (ulong)OpCode.BindRest << 56;
            bindRest += (ulong)lambdaExpr.Parameters.Required.Length;
            codes.Add(bindRest);
        }

        var body = CompileSequence(
            lambdaExpr.Bodies,
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

    public ulong[] CompileApplication(ParsedApplication app,
        CompileTimeEnvironment ctEnv,
        Sys.List<Form> literals,
        Sys.List<Binding> bindings,
        Context context,
        int scopeLevel,
        int startLine = 0)
    {
        Sys.List<ulong> instructions = [];
        var xs = app.ParsedExprs.ToArray();
        // we're going to wait til the end to make the push continuation instr because we need to know the address
        // we'll insert it at the front
        int lineNo = startLine;
        // eval and push for all args to the call
        for (int i = xs.Length - 1; i > 0; i--) {
            var codes = Compile(xs[i], ctEnv, literals, bindings, Context.Argument, scopeLevel, context == Context.Tail ? lineNo : lineNo + 1).ToList();
            // codes.Add((ulong)OpCode.Push << 56);
            instructions = instructions.Concat(codes).ToList();
            lineNo += codes.Count();
        }

        var codeForProc = Compile(xs[0], ctEnv, literals, bindings, Context.Argument, scopeLevel, lineNo);
        // don't push it. Call assumes the procedure is in VAL
        instructions.AddRange(codeForProc);
        lineNo += codeForProc.Length;
        
        instructions.Add((ulong)OpCode.Call << 56);
        lineNo++;
        
        if (context != Context.Tail) {
            // the call is not a tail call, so we _do_ want to save a continuation before we start
            var pushContInstruction =
                (context == Context.Argument
                    ? (ulong)OpCode.PushContinuationForArg
                    : (ulong)OpCode.PushContinuationForNonTailBody) << 56;
            pushContInstruction += (ulong)lineNo + 1;
            instructions.Insert(0, pushContInstruction);
            // ReSharper disable once DuplicatedStatements
            return instructions.ToArray();
        }

        return instructions.ToArray();

    }
    public Template CompileFile(ParsedExpr[] parsedFile, CompileTimeEnvironment cte) {
        
        Sys.List<Form> literals = [];
        Sys.List<Binding> bindings = [];

        Sys.List<ulong> instructions = [];

        int lineNo = 0;
        foreach (var x in parsedFile.Take(parsedFile.Length - 1)) {
            instructions = instructions.Concat(Compile(x, cte, literals, bindings, Context.NonTailBody, 0, lineNo)).ToList();
            lineNo += instructions.Count();
        }
        // add instruction for expr in tail position
        instructions = instructions.Concat(Compile(parsedFile[parsedFile.Length - 1], cte, literals, bindings, Context.Tail, 0, lineNo)).ToList();
        return new Template(0, instructions.ToArray(), bindings.ToArray(), literals.ToArray(), 0, false);
    }
}