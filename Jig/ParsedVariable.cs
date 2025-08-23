using System.Diagnostics.CodeAnalysis;

namespace Jig;

public class ParsedVariable : ParsedForm {

    public static bool TryParse(Syntax stx, MacroExpander expander, [NotNullWhen(returnValue: true)] out ParsedVariable? parsedVariable) {
        // if (stx is ParsedVariable pvar) {
        //     if(expander.TryResolve(pvar.Identifier, out Binding? binding)) {
        //         pvar.Identifier.Symbol.Binding = binding;
        //         parsedVariable = new ParsedVariable.Lexical(pvar.Identifier, stx.SrcLoc);
        //         return true;
        //     } else {
        //         parsedVariable = pvar;
        //         return true;
        //     }

        // }
        // TODO: make binding a required field for parsed variables
        if (stx is Identifier id) {
            if(expander.TryResolve(id, out var binding)) {
                // if (id.Symbol.Name == "y") {
                //     Console.WriteLine($"found binding for y: {binding}");
                // }
                id.Symbol.Binding = binding;
                if (binding.ScopeLevel == 0) {
                    parsedVariable = new ParsedVariable.TopLevel(id, binding, stx.SrcLoc);
                } else {
                    parsedVariable = new ParsedVariable.Lexical(id, binding, stx.SrcLoc);
                }
                return true;
            }

            throw new Exception("expander: could not resolve binding for {id}");

        }
        parsedVariable = null;
        return false;
    }

    private ParsedVariable(Identifier id, Binding binding, SrcLoc? srcLoc) : base (id.Symbol, srcLoc) {
        Binding = binding;
        Identifier = id;
    }

    public class TopLevel : ParsedVariable {
        internal TopLevel(Identifier id, Binding binding, SrcLoc? srcLoc) : base (id, binding, srcLoc) {}
    }

    public class Lexical : ParsedVariable {
        internal Lexical(Identifier id, Binding binding, SrcLoc? srcLoc) : base(id, binding, srcLoc) {
        }
        

    }

    public Identifier Identifier { get; }
    public Binding Binding { get; }
}