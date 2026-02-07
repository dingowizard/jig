namespace Jig.Expansion;

public partial class CoreParseRules
{
    // TODO: should these all return the more specific type (here, ParsedBegin)?
    public static SemiParsedForm ParseBeginForm(Syntax syntax, ExpansionContext context) {
        var stxList = (SyntaxList)Syntax.E(syntax);
        var subForms = stxList.ToArray<Syntax>();
        if (!context.DefinesAllowed) {
            if (subForms.Length < 2) {
                throw new Exception($"invalid syntax in begin @ {syntax.SrcLoc}: expected at least one expression");
            } 
        }
        SemiParsedForm[] semiParsedForms = context.Expander.ExpandSequenceFirstPass(stxList.Skip<Syntax>(1), context);
        // System.Collections.Generic.List<SemiParsedForm> semiParsedForms = [];
        // (semiParsedForms, _) = SemiParseBeginSequence(subForms.Skip(1), semiParsedForms, context);
        //
        //
        // // Console.WriteLine($"returning ParsedBeginForm. semiParsed.Count = {semiParsedForms.Count()} semiParsed = {string.Join(", ", semiParsedForms.Select(s => s.Print()))}, stxList = {stxList.Print()}");
        bool hasExpression = false;
        foreach (var semiParsedForm in semiParsedForms) {
            if (semiParsedForm is not SemiParsedDefine && semiParsedForm is not SemiParsedDefineSyntax) {
                hasExpression = true;
            }
        }
        return new SemiParsedBegin(semiParsedForms, stxList, hasExpression, syntax.SrcLoc);
    }
    // private static (System.Collections.Generic.List<SemiParsedForm>, ExpansionContext) SemiParseBeginSequence (
    //     IEnumerable<Syntax> subForms,
    //     System.Collections.Generic.List<SemiParsedForm> semiParsedForms,
    //     ExpansionContext context)
    // {
    //     var argType = context.Type == ExpansionContextType.Argument;
    //     // if (argType) {
    //     //     Console.WriteLine($"SemiParseBeginSequence in arg context: {string.Join(", ", subForms.Select(f => f.Print()))}");
    //     // }
    //     System.Collections.Generic.List<SemiParsedForm> result = [];
    //     foreach (var syntax in subForms) {
    //         (bool isBegin, Syntax macroExpanded) = MacroExpandForBegin(syntax, context);
    //         if (isBegin) {
    //             Syntax[] nestedSubForms = ((SyntaxList)Syntax.E(macroExpanded)).Skip<Syntax>(1).ToArray<Syntax>();
    //             System.Collections.Generic.List<SemiParsedForm> nestedBegin = [];
    //             (nestedBegin, context) = SemiParseBeginSequence(nestedSubForms, semiParsedForms, context);
    //             result.AddRange(nestedBegin);
    //             continue;
    //         }
    //         if (macroExpanded is SemiParsedForm semiParsedForm) {
    //             if (context.Type is ExpansionContextType.LibraryBody or ExpansionContextType.LambdaBody &&
    //                 (semiParsedForm is not SemiParsedDefine ||
    //                 semiParsedForm is not SemiParsedDefineSyntax)) {
    //                 // the begin is a form in a lambda body or a library body
    //                 // and it is the first expression after the definitions
    //                 context = context.ExtendWithExpressionContext();
    //             }
    //             // Console.WriteLine($"in SemiParseBeginSequence: adding {semiParsedForm.Print()}");
    //             result.Add(semiParsedForm);
    //             continue;
    //         }
    //         throw new Exception($"expected result of MAcroExpandForBegin to be a un-semi-parsed begin form syntax or a semi-parsed form. Got {macroExpanded.Print()} , a {macroExpanded.GetType()}");
    //     }
    //     // Console.WriteLine($"returning from SemiParseBeginSequence: result count = {result.Count()}");
    //     return (result, context);
    //
    // }
    /*private static (bool, Syntax) MacroExpandForBegin(Syntax syntax, ExpansionContext context) {
        while (Syntax.E(syntax) is SyntaxList.NonEmpty stxList) {
            // TODO: stop looping forever on begin in argument context
            // Console.WriteLine($"in MacroExpandForBegin: syntax = {syntax.Print()}");
            if (stxList.First is Identifier kw) {
                if (context.Expander.Owner.Keywords.TryFind(kw, out IExpansionRule? rule)) {
                    if (IsCoreSyntaxBegin(context, rule)) {
                        return (true, syntax);
                    }
                    var expanded = rule.Expand(syntax, context);
                    // TODO: expand for transformers should return Syntax, not ParsedForm
                    if (syntax is SemiParsedForm semi) {
                        return (false, semi);
                    }
                    // Console.WriteLine($"MacroExpandForBegin {syntax.Print()} => {expanded.Print()}. LOOP!");
                    syntax = expanded;
                    continue;
                } else {
                    // first is identifier but not keyword (so function application)
                    return (false, context.Expander.FirstPass(syntax, context));
                }
            } // first element of list was not an identifier. So maybe a lambda expression or an application?
            break;
        }
        return (false, context.Expander.FirstPass(syntax, context));
    }*/
    /*private static bool IsCoreSyntaxBegin(ExpansionContext context, IExpansionRule expansionRule) {
        IExpansionRule? beginRule = context.Runtime.CoreSyntax.Where(p => p.Item1.Name == "begin").Select(p => p.Item2).First();
        // TODO: cache beginRule somewhere that is accessible here so that we don't have to look this up a billion times
        if (beginRule is null) throw new Exception();
        return beginRule == expansionRule;

    }*/

}

public class SemiParsedBegin : SemiParsedForm {
    // NOTE: a SemiParsedBegin is neither an expression nor a definition
    // TODO: should we be able to decide whether or not it is a defn or expr during first pass
    // if so, there be two different forms, or is it sufficient just to error out
    // when a definition is encountered where it is not allowed?
    public SemiParsedBegin(IEnumerable<SemiParsedForm> semiParsed,
        SyntaxList stxList,
        bool hasExpression,
        SrcLoc? syntaxSrcLoc) : base(stxList, syntaxSrcLoc)
    {
        Keyword = (Identifier)stxList.ElementAt<Syntax>(0);
        SemiParsed = semiParsed;
        HasExpression = hasExpression;
    }
    public Identifier Keyword {get;}

    public IEnumerable<SemiParsedForm> SemiParsed {get;}
    public bool HasExpression {get;}

    public override ParsedForm SecondPass(ExpansionContext context) {
        // Console.WriteLine($"Begin second pass!");
        var fullyParsed = new System.Collections.Generic.List<ParsedForm>();
        foreach (var semiParsed in SemiParsed) {
            fullyParsed.Add(context.Expander.SecondPass(semiParsed, context));

        }
        return new ParsedBegin(
            Keyword,
            fullyParsed.ToArray<ParsedForm>(),
            SrcLoc);
    }
}