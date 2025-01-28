using Jig;
using Sys = System.Collections.Generic;

namespace VM;

public class Compiler {

    public Template Compile(
        ParsedExpr[] sequence,
        CompileTimeEnvironment ctEnv,
        Sys.List<Jig.Form> literals,
        Sys.List<Binding> globals,
        bool tail = false)
    {
        throw new NotImplementedException();
    }

    public ulong[] Compile(
        ParsedLiteral literal,
        Sys.List<Jig.Form> literals,
        bool tail = false)
    {
        var constExpr = (Form)Syntax.E(literal.Quoted);
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

    public ulong[] Compile(
        ParsedVariable.TopLevel var,
        CompileTimeEnvironment ctEnv,
        Sys.List<Binding> globals,
        bool tail = false)
    {
        Form.Symbol sym = var.Identifier.Symbol;
        if (!globals.Any(b => Equals(b.Symbol, sym))) {
            globals.Add(ctEnv.LookUpTopLevel(sym));
        }
        int index = globals.FindIndex(b => Equals(b.Symbol, sym));
        ulong code = (ulong)OpCode.Top << 56;
        code += (ulong)index;
        if (tail) {
            return [code, (ulong)OpCode.PopContinuation << 56];
        }
        return [code];

    }

    public ulong[] Compile(
        ParsedVariable.Lexical var,
        CompileTimeEnvironment ctEnv,
        bool tail = false) {

        // TODO: we already found the bindings when we parsed/expanded
        // we shouldn't have to do it twice
        (int frame, int slot) = ctEnv.LookUpLexVar(var.Identifier.Symbol);
        ulong code = (ulong)OpCode.LexVar << 56;
        code += (ulong)frame << 24;
        code += (ulong)slot;
        if (tail) {
            return [code, (ulong)OpCode.PopContinuation << 56];
        }
        return [code];
    }

}