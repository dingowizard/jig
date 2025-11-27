namespace Jig.Expansion;

public partial class CoreParseRules {

    public static SemiParsedForm ParseDefineForm(Syntax syntax, ExpansionContext context) {
        if (!context.DefinesAllowed) {
            throw new Exception($"definition encountered in expression context: {syntax.Print()}. context type is {context.Type}");
        }
        // NOTE: these are first pass expander tasks
        if (Syntax.E(syntax) is not SyntaxList.NonEmpty stxList) {
            // TODO: shouldn't need to test for this again.
            throw new Exception($"malformed define {syntax.Print()}");
        }
        
        if (stxList.Rest is not SyntaxList.NonEmpty { First: Identifier id } rest)
            throw new Exception($"malformed define: {Syntax.ToDatum(syntax).Print()} ");
        Parameter binding;
        if (context.TryResolve(id, out var parameter) && context.ScopeLevel == 0) {
            // this is a top-level/module-level define, so maybe we are re-defining 
            // TODO: whether or not we're allowed to redefine something that already has
            // a binding depends on the context.
            // if we're in the repl and re-defining a top-level at top-level, that's fine
            // otherwise we should make a new binding
            // or raise an error
            binding = parameter;
        } else {
            // TODO: this represents registering the new var
            // if its a toplevel and the rhs fails to parse, how do we remove this (or not add) it to the environment?
            binding = new Parameter(
                id.Symbol,
                id.ScopeSet,
                context.ScopeLevel,
                context.VarIndex++,
                id.SrcLoc);
            context.AddBinding(binding);
        }
        // var parsedVar = new ParsedVariable.TopLevel(id, binding, id.SrcLoc);
        return new SemiParsedDefine(
            SyntaxList
                .FromParams(stxList.First, binding)
                .Concat<Syntax>(rest.Rest)
                .ToSyntaxList(),
            syntax.SrcLoc);

    }
}

public class SemiParsedDefine : SemiParsedDefinition {
    public SemiParsedDefine(SyntaxList toSyntaxList, SrcLoc? syntaxSrcLoc) : base(toSyntaxList, syntaxSrcLoc) {
        var subForms = toSyntaxList.ToArray<Syntax>();
        int formLength = subForms.Length;
        if (formLength is not (2 or 3)) {
            throw new Exception($"bad syntax in define @ {syntaxSrcLoc}: expected 2 or 3 sub-forms, got {formLength}: {toSyntaxList.Print()}");
        }
        Keyword = (Identifier)subForms[0];
        Var = subForms[1] as Parameter ?? throw new Exception($"ParseDefineForm: expected variable to have been parsed in first pass of expansion ");
        if (formLength == 3) {
            Expr = subForms[2];
        }
    }
    
    public Identifier Keyword {get;}
    public Parameter Var {get;}
    
    public Syntax? Expr {get;}
    public override ParsedForm SecondPass(ExpansionContext context) {
        
                return Expr is not null ?
                    new ParsedDefine(
                        Keyword,
                        Var,
                        context.ExtendWithExpressionContext().Expand(Expr), 
                        SrcLoc) :
                    new ParsedDefine(
                        Keyword,
                        Var,
                        SrcLoc);
            

    }
}
