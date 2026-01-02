using Jig;
using Jig.Expansion;
using Jig.IO;

namespace VM;

// TODO: does any of this require VM? can it be moved to Jig?
// VMFactory and Evaluator

public class Library : ILibrary {
    public IEnumerable<Binding> VariableExports {get;}
    // TODO: why are these two different?
    // Because keywords have to be bound to expansion rules
    public IEnumerable<(Symbol, IExpansionRule)> KeywordExports {get;}

    public Library(ParsedLibrary parsedLibrary, VMFactory vmFactory) {
        
        // this version of library evaluates the library doing first- and second-pass expansion
        // and executing its body code.
        
        var evaluator = (Evaluator)vmFactory.Build(); // TODO: hm
        evaluator.Import(parsedLibrary.ImportForm);
        if (parsedLibrary.Body.Count() != 0) {
            // TODO: maybe EvalSequence should not throw when the body is empty?
            evaluator.EvalSequence(parsedLibrary.Body);
        }
        // TODO: this is backwards. We should look up every exported symbol in the current environment
        // rather than filtering our anything not exported. Repeat below
        KeywordExports = evaluator.Keywords.Rules
                .Select(kvp => (kvp.Key, kvp.Value))
                .Where(tuple => parsedLibrary.Exports.Vars.Select(id => id.Symbol).Contains(tuple.Key)); ;
        VariableExports = evaluator.Variables.TopLevels.Values
                .Where(b => parsedLibrary.Exports.Vars.Select(id => id.Symbol)
                    .Contains(b.Parameter.Symbol));

    }

    public Library(ParsedLibrary parsedLibrary, Evaluator evaluator) {
        
        evaluator.Import(parsedLibrary.ImportForm);
        if (parsedLibrary.Body.Count() != 0) {
            // TODO: maybe EvalSequence should not throw when the body is empty?
            evaluator.EvalSequence(parsedLibrary.Body);
        }
        KeywordExports = evaluator.Keywords.Rules
            .Select(kvp => (kvp.Key, kvp.Value))
            .Where(tuple => parsedLibrary.Exports.Vars.Select(id => id.Symbol).Contains(tuple.Key)); ;
        VariableExports = evaluator.Variables.TopLevels.Values
            .Where(b => parsedLibrary.Exports.Vars.Select(id => id.Symbol)
                .Contains(b.Parameter.Symbol));
    }
    

    public static ILibrary FromForm(ParsedLibrary parsedLibraryForm, VMFactory vmFactory) {

        return new Library(parsedLibraryForm, vmFactory);
    }
    
    public static Library FromFile(string path, Func<InputPort, Syntax> reader, IEvaluatorFactory evaluatorFactory) {
        
        Evaluator evaluator = (Evaluator)evaluatorFactory.Build();
        var port = new InputPort(path);
        var stx = reader(port);
        var context = new ExpansionContext(evaluator, evaluator.Variables.TopLevels.Keys, ExpansionContextType.REPL);
        var program = evaluator.Expander.ExpandREPLForm(stx, context);
        if (program is not ParsedLibrary parsedLibrary) throw new Exception($"expected library form, got {program}");

        return new Library(parsedLibrary, evaluator);

    }

    static Library() {
        int index = 0;
        Binding[] coreBindings =
        [
            new (new Parameter(new Symbol("cons"), [], 0, index++, null),
                new Location(new Primitive2(Primitives.cons2, 2, false))),
            new (new Parameter(new Symbol("error"), [], 0, index++, null),
                new Location(SchemeValue.Void)),
            new (new Parameter(new Symbol("unchecked-car"), [], 0, index++, null),
                new Location(Primitives.Car)),
            new (new Parameter(new Symbol("unchecked-cdr"), [], 0, index++, null),
                new Location(Primitives.Cdr)),
            new (new Parameter(new Symbol("append"), [], 0, index++, null),
                new Location(Primitives.Append)),
            new (new Parameter(new Symbol("pair?"), [], 0, index++, null),
                new Location(Primitives.PairP)),
            // new (new Parameter(new Symbol("list?"), [], 0, index++, null),
            //     new Location(Primitives.ListP)),
            new (new Parameter(new Symbol("null?"), [], 0, index++, null),
                new Location(Primitives.NullP)),
            // new (new Parameter(new Symbol("zero?"), [], 0, index++, null),
            //     new Location(Primitives.ZeroP)),
            // TODO: Default environment isn't used and should be empty?
            new (new Parameter(new Symbol("number?"), [], 0, index++, null),
                new Location(new Primitive("number?", Primitives.numberP, 1, false))),
            new (new Parameter(new Symbol("procedure?"), [], 0, index++, null),
                new Location(new Primitive("procedure?", Primitives.procedureP, 1, false))),
            new (new Parameter(new Symbol("call/cc"), [], 0, index++, null),
                new Location(new Procedure(Environment.Default, Builtins.CallCC))),
            new (new Parameter(new Symbol("unchecked-bin-op-+"), [], 0, index++, null),
                new Location(new Procedure(Environment.Default, Builtins.BinOpPlus))),
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
                new Location(Primitives.SyntaxE)), // NOTE: this is not a std scheme procedure
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
            new (new Parameter(new Symbol("expand"), [], 0, index++, null),
                new Location(new Primitive("expand", Primitives.expand, 1, false))),
        ];

        // env._dict.Add(new Symbol("record-constructor"), new Procedure( (Builtin)Builtins.record_constructor));
        (Symbol, IExpansionRule)[] keywords =
        [
            
            // TODO: it should be necessary to export the core syntactic forms (lambda, set! , define, etc ...)
            // but for some reason these are available everywhere

            new (new Symbol("and"), new BuiltinTransformer(BuiltinTransformer.and)),
            new (new Symbol("quasiquote"), new BuiltinTransformer(BuiltinTransformer.quasiquote)),
            new (new Symbol("syntax-rules"), new BuiltinTransformer(BuiltinTransformer.syntax_rules)),
        ];
        Core = new Library(coreBindings, keywords);
    }
    public static Library Core {get;}


    private Library(IEnumerable<Binding> vars, IEnumerable<(Symbol, IExpansionRule)> keywords) {
        VariableExports = vars;
        KeywordExports = keywords;
    }

    public Parameter FindParameter(string name) {
        foreach (Binding binding in VariableExports) {
            if (binding.Parameter.Symbol.Name == name) {
                return binding.Parameter;
            }
        }
        throw new Exception($"couldn't find parameter with name = {name} in Library");
    }

}