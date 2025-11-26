using System.Diagnostics;
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


        var vars = evaluator.Variables;
        if (vars is null) throw new Exception($"evaluator.Variables was null");
        var toplevels = vars.TopLevels;
        if (toplevels is null) throw new Exception($"TopLevels was null");
        var importedVars = evaluator.Variables.TopLevels.Keys.ToArray();
        var importedKeywords = evaluator.Keywords.Rules.Keys.ToArray();

        var port = new InputPort(path);
        var stxes = reader(port);
        evaluator.EvalSequence(stxes);
        // this is crude until we get importing and exporting specific bindings. Just don't export anything you imported
        var varsToExport =
            evaluator
                .Variables
                .TopLevels
                .Values
                .Where(b => !importedVars.Contains(b.Parameter));

        var kwsToExport = new System.Collections.Generic.List<(Symbol, IExpansionRule)>();
        foreach (var k in evaluator.Keywords.Rules)
        {
            if (!importedKeywords.Contains(k.Key)) {
                kwsToExport.Add((k.Key, k.Value));
            }
        }
        

        return new Library(varsToExport, kwsToExport);

    }

    static Library()
    {
        var index = 0;
        Binding[] coreBindings =
        [ 
            new (new Parameter(new Symbol("cons"), [], 0, index++, null),
                    new Location(new Primitive2(Primitives.cons2, 2, false))),
            new (new Parameter(new Symbol("car"), [], 0, index++, null),
                    new Location(Primitives.Car)),
            new (new Parameter(new Symbol("cdr"), [], 0, index++, null),
                    new Location(Primitives.Cdr)),
            new (new Parameter(new Symbol("append"), [], 0, index++, null),
                    new Location(Primitives.Append)),
            new (new Parameter(new Symbol("pair?"), [], 0, index++, null),
                    new Location(Primitives.PairP)),
            new (new Parameter(new Symbol("list?"), [], 0, index++, null),
                    new Location(Primitives.ListP)),
            new (new Parameter(new Symbol("null?"), [], 0, index++, null),
                    new Location(Primitives.NullP)),
            new (new Parameter(new Symbol("zero?"), [], 0, index++, null),
                    new Location(Primitives.ZeroP)),
            // TODO: Default environment isn't used and should be empty?
            new (new Parameter(new Symbol("call/cc"), [], 0, index++, null),
                    new Location(new Procedure(Environment.Default, Builtins.CallCC))),
            new (new Parameter(new Symbol("+"), [], 0, index++, null),
                    new Location(new Procedure(Environment.Default, Builtins.Sum))),
            new (new Parameter(new Symbol("apply"), [], 0, index++, null),
                    new Location(new Procedure(Environment.Default, Builtins.Apply))),
                    // new Location(Primitives.Apply)),
            // new (new Parameter(new Symbol("expand"), 0, index++, null),
            //         new Location(Primitives.Expand)),
            new (new Parameter(new Symbol(">"), [], 0, index++, null),
                    new Location(Primitives.GT)),
            new (new Parameter(new Symbol("<"), [], 0, index++, null),
                    new Location(Primitives.LT)),
            new (new Parameter(new Symbol("-"), [], 0, index++, null),
                    new Location(Primitives.Minus)),
            new (new Parameter(new Symbol("*"), [], 0, index++, null),
                    new Location(new Procedure(Environment.Default, Builtins.Product))),
            new (new Parameter(new Symbol("="), [], 0, index++, null),
                    new Location(Primitives.NumEq)),
            new (new Parameter(new Symbol("eqv?"), [], 0, index++, null),
                    new Location(Primitives.Eqvp)),
            new (new Parameter(new Symbol("values"), [], 0, index++, null),
                    new Location(new Primitive("values", Primitives.values, 0, true))),
            new (new Parameter(new Symbol("call-with-values"), [], 0, index++, null),
                    new Location(new Procedure(Environment.Default, Builtins.CallWithValues))),
            new (new Parameter(new Symbol("dynamic-wind"), [], 0, index++, null),
                    new Location(new Procedure(Environment.Default, Builtins.DynamicWind))),
            new (new Parameter(new Symbol("datum->syntax"), [], 0, index++, null),
                            new Location(Primitives.DatumToSyntax)),
            new (new Parameter(new Symbol("syntax->datum"), [], 0, index++, null),
                            new Location(Primitives.SyntaxToDatum)),
            new (new Parameter(new Symbol("syntax->list"), [], 0, index++, null),
                            new Location(Primitives.SyntaxToList)),
            new (new Parameter(new Symbol("syntax-e"), [], 0, index++, null),
                            new Location(Primitives.SyntaxE)),// NOTE: this is not a std scheme procedure
            new (new Parameter(new Symbol("symbol?"), [], 0, index++, null),
                            new Location(Primitives.SymbolP)),
            new (new Parameter(new Symbol("symbol=?"), [], 0, index++, null),
                    new Location(Primitives.SymbolEqualP)),
            new (new Parameter(new Symbol("displayln"), [], 0, index++, null),
                    new Location(Primitives.DisplayLine)), // NOTE: this is not a std scheme procedure
            new (new Parameter(new Symbol("display"), [], 0, index++, null),
                    new Location(Primitives.Display)),
            new (new Parameter(new Symbol("newline"), [], 0, index++, null),
                    new Location(Primitives.NewLine)),
            new (new Parameter(new Symbol("vector"), [], 0, index++, null),
                    new Location(new Primitive("vector", Primitives.vector, 0, true))),
            new (new Parameter(new Symbol("vector-ref"), [], 0, index++, null),
                    new Location(new Primitive("vector-ref", Primitives.vectorRef, 2, false))),
            new (new Parameter(new Symbol("vector?"), [], 0, index++, null),
                    new Location(new Primitive("vector?", Primitives.vectorP, 1, false))),
            new (new Parameter(new Symbol("vector-length"), [], 0, index++, null),
                    new Location(new Primitive("vector-length", Primitives.vectorLength, 1, false))),
            new (new Parameter(new Symbol("make-record-type-descriptor"), [], 0, index++, null),
                    new Location(new Primitive("make-record-type-descriptor", Primitives.make_record_type_descriptor, 0, true))),
            new (new Parameter(new Symbol("record-type-descriptor?"), [], 0, index++, null),
                    new Location(new Primitive("record-type-descriptor?", Primitives.record_type_descriptor_p, 0, true))),
            new (new Parameter(new Symbol("record-constructor-descriptor?"), [], 0, index++, null),
                    new Location(new Primitive("record-constructor-descriptor?", Primitives.record_constructor_descriptor_p, 1, false))),
            new (new Parameter(new Symbol("make-record-constructor-descriptor"), [], 0, index++, null),
                    new Location(new Primitive("make-record-constructor-descriptor", Primitives.make_record_constructor_descriptor, 0, true))),
            new (new Parameter(new Symbol("record?"), [], 0, index++, null),
                    new Location(new Primitive("record?", Primitives.record_p, 1, false))),
            new (new Parameter(new Symbol("record-predicate"), [], 0, index++, null),
                    new Location(new Primitive("record-predicate", Primitives.record_predicate, 1, false))),
            new (new Parameter(new Symbol("record-accessor"), [], 0, index++, null),
                    new Location(new Primitive("record-accessor", Primitives.record_accessor, 2, false))),
            new (new Parameter(new Symbol("record-constructor"), [], 0, index++, null),
                    new Location(new Primitive("record-constructor", Primitives.record_constructor, 1, false))),
        ];
        
            // env._dict.Add(new Symbol("record-constructor"), new Procedure( (Builtin)Builtins.record_constructor));
        (Symbol, IExpansionRule)[] keywords = [
        
            new (new Symbol("and"), new BuiltinTransformer(BuiltinTransformer.and)),
            new (new Symbol("quasiquote"), new BuiltinTransformer(BuiltinTransformer.quasiquote)),
            new (new Symbol("syntax-rules"), new BuiltinTransformer(BuiltinTransformer.syntax_rules)),
        ];
        Core = new Library(coreBindings, keywords);
    }
    public static Library Core { get; }


    public Library(IEnumerable<Binding> vars, IEnumerable<(Symbol, IExpansionRule)> keywords) {
            _variableExports = new Lazy<IEnumerable<Binding>>(() => vars);
            KeywordExports = keywords;
    }

    public Library(ParsedLibrary parsedLibrary, IEnumerable<(Symbol, IExpansionRule)> keywords) {

            _variableExports = new Lazy<IEnumerable<Binding>>(() => []);
            KeywordExports = keywords;
            
    }

    public Parameter FindParameter(string name) {
            foreach (var binding in VariableExports) {
                    if (binding.Parameter.Symbol.Name == name) {
                            return binding.Parameter;
                    }
            }
            throw new Exception($"couldn't find parameter with name = {name} in Library");
    }

    public IEnumerable<Binding> VariableExports => _variableExports.Value;

    private Lazy<IEnumerable<Binding>> _variableExports;
    
    public IEnumerable<(Symbol, IExpansionRule)> KeywordExports { get; }
    
    
}