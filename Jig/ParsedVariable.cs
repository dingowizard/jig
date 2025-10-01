using System.Diagnostics.CodeAnalysis;

namespace Jig;

public class ParsedVariable : ParsedForm {


    private ParsedVariable(Identifier id, Expansion.Binding binding, SrcLoc? srcLoc) : base (id.Symbol, srcLoc) {
        Binding = binding;
        Identifier = id;
    }

    public class TopLevel : ParsedVariable {
        internal TopLevel(Identifier id, Expansion.Binding binding, SrcLoc? srcLoc) : base (id, binding, srcLoc) {}
    }

    public class Lexical : ParsedVariable {
        internal Lexical(Identifier id, Expansion.Binding binding, SrcLoc? srcLoc) : base(id, binding, srcLoc) {
        }
        

    }

    public Identifier Identifier { get; }
    public Expansion.Binding Binding { get; }
}