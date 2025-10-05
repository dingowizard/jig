using Jig;
using Jig.Expansion;
using Sys = System.Collections.Generic;

namespace VM;

public class Compiler {

    public Template CompileExprForREPL(ParsedForm x,
        Environment ctEnv,
        int scopeLevel = 0,
        int startLine = 0) {

        Sys.List<SchemeValue> literals = [];
        Sys.List<Parameter> bindings = [];

        DoFirstPassOneForm(bindings, x, ctEnv);
        ulong[] code = Compile(x, ctEnv, literals, bindings, Context.Tail, scopeLevel, startLine);
        if (code.Length == 0) {
            code = [(ulong)OpCode.PopContinuation << 56];
        }
        var result = new Template(0, code,bindings.ToArray(), literals.ToArray(), 0, false); // TODO: maybe there should be a different kind of template for this, since we don't need parameters
        // Array.ForEach(Disassembler.Disassemble(result), Console.WriteLine);
        return result;

    }

    private ulong[] Compile(ParsedForm x,
        Environment ctEnv,
        Sys.List<SchemeValue> literals,
        Sys.List<Parameter> bindings,
        Context context,
        int scopeLevel,
        int startLine = 0) {
        switch (x) {
            case ParsedLiteral lit:
                return CompileLit(lit, literals, context);
            case ParsedVariable.TopLevel top:
                return CompileTop(top, ctEnv, bindings, context);
            case ParsedVariable.Lexical lexVar:
                return CompileLexVar(lexVar, bindings, context, scopeLevel);
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
            case ParsedDefineSyntax defineSyntax:
                return CompileDefineSyntax(defineSyntax);
            default:
                throw new NotImplementedException($"{x.Print()} of type {x.GetType()} is not supported yet");
        }
    }

    private ulong[] CompileDefineSyntax(ParsedDefineSyntax defineSyntax) {
        // TODO: Shouldn't this compile to a pop continuation if it is tail?
        return [];
    }

    private ulong[] CompileBegin(ParsedBegin begin, Environment ctEnv, Sys.List<SchemeValue> literals,
        Sys.List<Parameter> bindings,
        Context context, int scopeLevel, int startLine)
    {
        // NOTE: wrong type of begin in wrong context is assumed to have been caught as a syntax error
        var sequence = begin.Forms;
        if (scopeLevel == 0) {
            // this is a top level begin and any defines are top level, so ...
            DoFirstPass(bindings, sequence, ctEnv);
        }
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
        Environment ctEnv,
        Sys.List<SchemeValue> literals,
        Sys.List<Parameter> bindings,
        Context context,
        int scopeLevel,
        int startLine = 0)
    {
        if (setForm.Variable.Parameter.ScopeLevel == scopeLevel) {
            return [((ulong)OpCode.SetArg << 56) + (ulong)setForm.Variable.Parameter.Index];
        }
        Sys.List<ulong> result = new();
        var bing = setForm.Variable.Parameter;
        int index = bindings.IndexOf(bing);
        if (index == -1) {
            bindings.Add(bing);
            index = bindings.Count - 1;
        }
        ulong code = (ulong)OpCode.SetTop << 56;
        code += (ulong)index;
        result.Add(code);

        // We have to compile the lambda function after the variable,
        // because there might be a recursive call to a toplevel
        result.InsertRange(0, Compile(setForm.Value, ctEnv, literals, bindings, Context.Argument, scopeLevel, startLine));
        // TODO: should this be CodeForContext?
        if (context == Context.Tail) {
            result.Add((ulong)OpCode.PopContinuation << 56);
        }
        return result.ToArray();
    }
    
    private ulong[] CompileDefinition(ParsedDefine defForm,
        Environment ctEnv,
        Sys.List<SchemeValue> literals,
        Sys.List<Parameter> bindings,
        Context context,
        int scopeLevel,
        int startLine = 0)
    {
        Sys.List<ulong> result = new();
        if (defForm.Variable is ParsedVariable.TopLevel topVar) {
            // TODO: this is not necessary when CompileDefinition is called by CompileBody

            var bing = bindings.Find(b => b.Symbol.Equals(topVar.Identifier.Symbol));
            ulong code = (ulong)OpCode.SetTop << 56;
            int index = bindings.IndexOf(bing);
            code += (ulong)index;
            result.Add(code);
        } else {
            var lexVar = (ParsedVariable.Lexical)defForm.Variable;
            ulong code;
            if (lexVar.Parameter.ScopeLevel == scopeLevel) {
                code = ((ulong)OpCode.SetArg << 56);
            } else {
                code = (ulong)OpCode.SetLex << 56;
            }
            int index = lexVar.Parameter.Index;
            code += (ulong)index;
            result.Add(code);
        }

        // We have to compile the lambda function after the variable,
        // because there might be a recursive call to a toplevel
        if (defForm.Value is not null) {
            result.InsertRange(0, Compile(defForm.Value, ctEnv, literals, bindings, Context.Argument, scopeLevel, startLine));
        } else {
            // push literal void in cases like "(define a)"
            if (!literals.Contains(SchemeValue.Void)) {
                literals.Add(SchemeValue.Void); 
            }
            int index = literals.IndexOf(SchemeValue.Void);
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
        Environment ctEnv,
        Sys.List<SchemeValue> literals,
        Sys.List<Parameter> bindings,
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

    public Template CompileSequence(ParsedForm[] sequence,
        Environment ctEnv,
        Sys.List<SchemeValue> literals,
        Sys.List<Parameter> bindings,
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
        Sys.List<SchemeValue> literals,
        Context context)
    {
        var constExpr = (SchemeValue)Syntax.ToDatum(literal.Quoted);
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
        Environment ctEnv,
        Sys.List<Parameter> bindings,
        Context context)
    {

        var parameter = var.Parameter;
        int index = bindings.FindIndex(p => p.Equals(parameter));
        if (index == -1) {
            bindings.Add(parameter);
            index = bindings.Count - 1;
        }
        // TODO: it looks like we're not going to need separate instructions for Top and LexVars
        // TODO: and probably don't need separate functions for compiling lex vars and top vars
        ulong code = (ulong)OpCode.Top << 56;
        code += (ulong)index;
        return CodeForContext(code, context);

    }

    public ulong[] CompileLexVar(
        ParsedVariable.Lexical var,
        Sys.List<Parameter> bindings,
        Context context,
        int scopeLevel )
    {

        if (var.Parameter.ScopeLevel == scopeLevel) {
            // this is an argument or a variable declared in a lambda scope
            return CodeForContext(
                ((ulong)OpCode.Arg << 56) + (ulong)var.Parameter.Index,
                context);
        }
        var parameter = var.Parameter;
        int index = bindings.IndexOf(parameter);
        if (index == -1) {
            bindings.Add(parameter);
            index = bindings.Count - 1;
        }
        ulong code = (ulong)OpCode.Top << 56;
        code += (ulong)index;
        // NOTE: this code for looking up how many ENVT frames the parameter is from the reference
        // will need to be re-created in the function that creates a closure
        // from an environment and a template
        
        // ulong code = (ulong)OpCode.Lex << 56;
        // int depth = scopeLevel - var.Parameter.ScopeLevel;
        // code += ((ulong)depth) << 32; // TODO: this could be too big I suppose.
        //
        // code += (ulong)var.Parameter.VarIndex;
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

    public ulong[] CompileLambdaExpr(ParsedLambda lambdaExpr,
        Environment ctEnv,
        Sys.List<SchemeValue> literals,
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
        Environment ctEnv,
        int scopeLevel)
    {
        var bindings = new Sys.List<Parameter>();
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
            new Sys.List<SchemeValue>(),
            bindings,
            ++scopeLevel,
            codes.Count());

        var scopeVars =
            lambdaExpr.Parameters.HasRest
                ? lambdaExpr.Parameters.Required.Append(lambdaExpr.Parameters.Rest).ToArray()
                : lambdaExpr.Parameters.Required;
        var result =
            new Template(
                lambdaExpr.ScopeVarsCount,
                codes.Concat(body.Code).ToArray(),
                body.Vars,
                body.Literals,
                lambdaExpr.Parameters.Required.Length,
                lambdaExpr.Parameters.HasRest);
        // Console.WriteLine($"***** {Syntax.ToDatum(lambdaExpr).Print()} compiled to: *****");
        // Array.ForEach(Disassembler.Disassemble(result), Console.WriteLine);
        return result;
        
    }

    public ulong[] CompileApplication(ParsedApplication app,
        Environment ctEnv,
        Sys.List<SchemeValue> literals,
        Sys.List<Parameter> bindings,
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
    public Template CompileFile(ParsedForm[] parsedFile, Environment cte) {
        
        Sys.List<SchemeValue> literals = [];
        Sys.List<Parameter> bindings = [];

        Sys.List<ulong> instructions = [];

        // NOTE: DoFirstPass is necessary for procedure definitions that have references
        // to variables that will be defined later in the file,
        // for example, in definitions of mutually recursive functions
        // TODO: how can we have this in the repl?
        
        DoFirstPass(bindings, parsedFile,cte); // make bindings for toplevels in scope first

        int lineNo = 0;
        foreach (var x in parsedFile.Take(parsedFile.Length - 1)) {
            instructions = instructions.Concat(Compile(x, cte, literals, bindings, Context.NonTailBody, 0, lineNo)).ToList();
            lineNo += instructions.Count();
        }
        // add instruction for expr in tail position
        var last = Compile(parsedFile[^1], cte, literals, bindings, Context.Tail, 0, lineNo);
        if (last.Length == 0)
        {
            // NOTE: the last form might be something that compiles to no instructions, like define-syntax
            last = [(ulong)OpCode.PopContinuation << 56];
        }
        instructions = instructions.Concat(last).ToList();
        return new Template(
            0,
            instructions.ToArray(),
            bindings.ToArray(),
            literals.ToArray(),
            0,
            false);
    }

    private void DoFirstPassOneForm(Sys.List<Parameter> bindings, ParsedForm form, Environment ctEnv) {
        if (form is ParsedDefine def)
        {
            var p = def.Variable.Parameter;
            if (!ctEnv.TopLevels.ContainsKey(p))
            {
                ctEnv.TopLevels.Add(p, new Binding(p, new Location()));
            }
            
            if (!bindings.Contains(p)) {
                bindings.Add(p);
            }
                
        }
    }

    private void DoFirstPass(Sys.List<Parameter> bindings, ParsedForm[] parsedFile, Environment ctEnv) {
        foreach (var form in parsedFile) {
            DoFirstPassOneForm(bindings, form, ctEnv);
            
        }
    }
}