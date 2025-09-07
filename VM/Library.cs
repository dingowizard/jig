using Jig;
using Jig.Expansion;
using Jig.IO;

namespace VM;

public class Library : ILibrary
{

    public static Library FromFile(
        string path,
        Func<InputPort, IEnumerable<Syntax>> reader,
        IEvaluatorFactory evaluatorFactory,
        IEnumerable<ILibrary> imports)
    {
        // TODO: we need to make a reader interface or instance methods or something
        IEvaluator evaluator = evaluatorFactory.Build();
        foreach (var import in imports) {
            evaluator.Import(import);
            // TODO: should specify in arguments which phases imports go to
            evaluator.Import(import, 1);
        }
        

        var importedVars = evaluator.Variables.TopLevels.Keys.ToArray();
        var importedKeywords = evaluator.Keywords.Rules.Keys.ToArray();

        var port = new InputPort(path);
        var stxes = reader(port);
        evaluator.EvalSequence((x) => { }, stxes);
        // this is crude until we get importing and exporting specific bindings. Just don't export anything you imported
        var varsToExport =
            evaluator
                .Variables
                .TopLevels
                .Values
                .Where(b => !importedVars.Contains(b.Symbol));

        var kwsToExport = new System.Collections.Generic.List<Tuple<Symbol, IExpansionRule>>();
        foreach (var k in evaluator.Keywords.Rules)
        {
            if (!importedKeywords.Contains(k.Key)) {
                kwsToExport.Add(new Tuple<Symbol, IExpansionRule>(k.Key, k.Value));
            }
        }
        

        return new Library(varsToExport, kwsToExport);

    }

    static Library()
    {
        Binding[] coreBindings =
        [ 
            new (new Symbol("cons"), Primitives.Cons),
            new (new Symbol("car"), Primitives.Car),
            new (new Symbol("cdr"), Primitives.Cdr),
            new (new Symbol("append"), Primitives.Append),
            new (new Symbol("pair?"), Primitives.PairP),
            new (new Symbol("list?"), Primitives.ListP),
            new (new Symbol("null?"), Primitives.NullP),
            new (new Symbol("zero?"), Primitives.ZeroP),
            // TODO: Default environment isn't used and should be empty?
            new (new Symbol("call/cc"), new Procedure(Environment.Default, VM.Builtins.CallCC)),
            new (new Symbol("+"), new Procedure(Environment.Default, VM.Builtins.Sum)),
            new (new Symbol("apply"), new Procedure(Environment.Default, VM.Builtins.Apply)),
            new (new Symbol("expand"), Primitives.Expand),
            new (new Symbol(">"), Primitives.GT),
            new (new Symbol("<"), Primitives.LT),
            new (new Symbol("-"), Primitives.Minus),
            new (new Symbol("*"), new Procedure(Environment.Default, VM.Builtins.Product)),
            new (new Symbol("="), Primitives.NumEq),
            new (new Symbol("eqv?"), Primitives.Eqvp),
            new (new Symbol("values"), new Procedure(Environment.Default, VM.Builtins.Values)),
            new (new Symbol("call-with-values"), new Procedure(Environment.Default, VM.Builtins.CallWithValues)),
            new (new Symbol("dynamic-wind"), new Procedure(Environment.Default, VM.Builtins.DynamicWind)),
            new (new Symbol("datum->syntax"), Primitives.DatumToSyntax),
            new (new Symbol("syntax->datum"), Primitives.SyntaxToDatum),
            new (new Symbol("syntax->list"), Primitives.SyntaxToList),
            new (new Symbol("syntax-e"), Primitives.SyntaxE),
            new (new Symbol("symbol?"), Primitives.SymbolP),
            new (new Symbol("symbol=?"), Primitives.SymbolEqualP)
        ];
        Tuple<Symbol, IExpansionRule>[] keywords = [
        
            new (new Symbol("and"), new BuiltinTransformer(BuiltinTransformer.and)),
            new (new Symbol("quasiquote"), new BuiltinTransformer(BuiltinTransformer.quasiquote)),
            new (new Symbol("syntax-rules"), new BuiltinTransformer(BuiltinTransformer.syntax_rules)),
        ];
        Core = new Library(coreBindings, keywords);
    }
    public static Library Core { get; }


    public Library(IEnumerable<IRuntimeBinding> vars, IEnumerable<Tuple<Symbol, IExpansionRule>> keywords)
    {
        VariableExports = vars;
        KeywordExports = keywords;
    }
    
    public IEnumerable<IRuntimeBinding> VariableExports { get; }
    
    public IEnumerable<Tuple<Symbol, IExpansionRule>> KeywordExports { get; }
    
    
}