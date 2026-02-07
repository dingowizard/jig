using Jig.Expansion;
namespace Jig;

public class ParsedLambda : Expression {

    internal ParsedLambda(Syntax keyword,
        LambdaParameters parameters,
        int scopeVarsCount,
        ParsedForm[] bodies,
        SrcLoc? srcLoc = null)
        : base((SchemeValue)Pair.Cons(keyword,
                (SchemeValue)Pair.Cons(parameters,
                    (SchemeValue)bodies.ToJigList())),
            srcLoc)
    {
        Parameters = parameters;
        Bodies = bodies;
        ScopeVarsCount = scopeVarsCount;
    }

    public LambdaParameters Parameters {get;}
    
    // number of parameters and local variables declared in body
    public int  ScopeVarsCount {get;}
    
    public ParsedForm[] Bodies {get;}


    public class LambdaParameters : Syntax {
        private LambdaParameters(Syntax stx, Parameter[] required, Parameter? rest)
            : base(Syntax.E(stx), stx.SrcLoc)
        {
            Required = required;
            Rest = rest;
        }


        public static LambdaParameters Parse(Syntax stx, ExpansionContext context) {
            var namesSeen = new System.Collections.Generic.List<string>();
            var required = new System.Collections.Generic.List<Parameter>();
            Parameter? rest = null;
            if (Syntax.E(stx) is SyntaxList psStxList) {
                foreach(Syntax p in psStxList.Cast<Syntax>()) {
                    Identifier id = p as Identifier ??
                                    throw new Exception($"lambda: expected parameters to be identifiers, but got {p} @ {p.SrcLoc?.ToString() ?? "?"}");
                    if (namesSeen.Contains(id.Symbol.Name)) {
                        throw new Exception($"lambda: expected parameters to have unique names but got {id} more than once @ {id.SrcLoc?.ToString() ?? "?"}");
                    }
                    Parameter parameter = new Parameter(
                        id.Symbol,
                        id.ScopeSet,
                        context.ScopeLevel,
                        context.VarIndex++,
                        id.SrcLoc);
                    id.Symbol.Binding = parameter;
                    context.AddBinding(parameter);
                    namesSeen.Add(id.Symbol.Name);
                    required.Add(parameter);
                }
            } else if (Syntax.E(stx) is IPair pair) {
                Identifier? id = pair.Car as Identifier;
                if (id is null) throw new Exception($"lambda: expected parameters to be identifiers, but got {pair.Car}");
                if (namesSeen.Contains(id.Symbol.Name)) {
                    throw new Exception($"lambda: expected parameters to have unique names but got {id} more than once @ {id.SrcLoc?.ToString() ?? "?"}");
                }
                namesSeen.Add(id.Symbol.Name);
                Parameter parameter =
                    new Parameter(
                        id.Symbol,
                        id.ScopeSet,
                        context.ScopeLevel,
                        context.VarIndex++,
                        id.SrcLoc);
                id.Symbol.Binding = parameter;
                required.Add(parameter);
                context.AddBinding(parameter);
                while (pair.Cdr is IPair cdrPair) {
                    id = cdrPair.Car as Identifier;
                    if (id is null) throw new Exception($"lambda: expected parameters to be identifiers, but got {pair.Car}");
                    if (namesSeen.Contains(id.Symbol.Name)) {
                        throw new Exception($"lambda: expected parameters to have unique names but got {id} more than once @ {id.SrcLoc?.ToString() ?? "?"}");
                    }
                    parameter =
                        new Parameter(
                            id.Symbol,
                            id.ScopeSet,
                            context.ScopeLevel,
                            context.VarIndex++,
                            id.SrcLoc);
                    id.Symbol.Binding = parameter;
                    context.AddBinding(parameter);
                    pair = cdrPair;
                    namesSeen.Add(id.Symbol.Name);
                    required.Add(parameter);
                }
                id = pair.Cdr as Identifier;
                if (id is null) throw new Exception($"lambda: expected parameters to be identifiers, but got {pair.Cdr}");
                if (namesSeen.Contains(id.Symbol.Name)) {
                    throw new Exception($"lambda: expected parameters to have unique names but got {id} more than once @ {id.SrcLoc?.ToString() ?? "?"}");
                }
                parameter = new Parameter(
                    id.Symbol,
                    id.ScopeSet,
                    context.ScopeLevel,
                    context.VarIndex++,
                    id.SrcLoc);
                id.Symbol.Binding = parameter;
                context.AddBinding(parameter);
                namesSeen.Add(id.Symbol.Name);
                rest = parameter;
            } else if (stx is Identifier psId) {
                Parameter parameter =
                    new Parameter(
                        psId.Symbol,
                        psId.ScopeSet,
                        context.ScopeLevel,
                        context.VarIndex++,
                        psId.SrcLoc);
                psId.Symbol.Binding = parameter;
                context.AddBinding(parameter);
                rest = parameter;
            } else if (Syntax.E(stx) is List.Empty) {

            } else {
                throw new Exception($"ExpandLambda: expected parameters to be list or identifier, got {Syntax.E(stx)}");
            }
            return new LambdaParameters(stx, required.ToArray(), rest);
        }
        public bool HasRequired => Required.Length != 0;

        public bool HasRest => Rest is not null;

        public Parameter[] Required { get; }
        public Parameter? Rest { get; }
    }
}