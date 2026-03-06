using System.Diagnostics.CodeAnalysis;
using Jig.Expansion;

namespace Jig;

public class ParsedVariable : Expression {


    private ParsedVariable(Identifier id, Expansion.Parameter parameter, SrcLoc? srcLoc) : base (id.Symbol, srcLoc) {
        Parameter = parameter;
        Identifier = id;
    }

    public class TopLevel : ParsedVariable {
        internal TopLevel(Identifier id, Expansion.Parameter parameter, SrcLoc? srcLoc) : base (id, parameter, srcLoc) {}
    }

    public class Lexical : ParsedVariable {
        internal Lexical(Identifier id, Expansion.Parameter parameter, SrcLoc? srcLoc) : base(id, parameter, srcLoc) {
        }
        

    }

    public Identifier Identifier { get; }
    public Expansion.Parameter Parameter { get; }

    public class MaybeTopLevel : TopLevel {
        public MaybeTopLevel(Identifier identifier, Parameter binding, SrcLoc? identifierSrcLoc) : base(identifier, binding, identifierSrcLoc) {
        }
    }
}