using Jig;
using Jig.Expansion;
using Jig.IO;

namespace VM;

public class Evaluator : IEvaluator<Machine> {

    public Evaluator(uint phase = 0) {
        Phase =  phase;
        Expander = new Expander(this, new VMFactory(Phase + 1));
        Variables = Environment = Environment.Minimal();
        Runtime = new Machine(this, (Environment)Variables);
        Keywords = Runtime.FreshCoreSyntax();

    }

    public uint Phase { get;  }

    public IExpansionRule EvaluateTransformerExpression(ParsedLambda transformerLambdaExpr, ExpansionContext context)
    {
        
        // TODO: needs a compiler, a compile-time environment
        if (Environment is null) throw new Exception($"unable to evaluate transformer expression: Environment was null.");
        if (Environment.TopLevels.Keys.Count == 0) {
            Console.WriteLine($"EvaluatoTransformExpression: empty env. phase is {this.Phase}");
            Console.WriteLine($"{transformerLambdaExpr.SrcLoc?.ToString() ?? "null"}");
            throw new Exception($"unable to evaluate transformer expression {transformerLambdaExpr.Print()}: Environment {Environment.GetHashCode()} is empty. owner phase = {context.Expander.Owner.Phase}");
        }
        var compiler = new Compiler(); // should class be static?
        var code = compiler.CompileExprForREPL(transformerLambdaExpr, Environment);
        ISchemeValue result = List.Null;
        Runtime.Load(code, Environment, Cont);
        Runtime.Run();
        Procedure proc = result as Procedure ?? throw new Exception("a transformer should evaluate to a procedure");
        return new Transformer(proc, Runtime);
        
        void Cont(SchemeValue[] forms) => result = forms[0];
    }
    
    
    // public Syntax ApplyTransformer(Jig.Expansion.Transformer transformer, Syntax syntax) {}
    IRuntime IEvaluator.Runtime => Runtime;

    public Expander Expander { get; }
    public IEvaluatorFactory Factory { get; } = new VMFactory();

    public SyntaxEnvironment Keywords { get; }

    public void REPLEval(ContinuationAny continuation, Syntax syntax)
    {
        var context = new ExpansionContext(this, Environment.TopLevels.Keys, ExpansionContextType.REPL);
        var program = Expander.ExpandREPLForm(syntax, context);
        
        // TODO: put compiler in runtime and hide loading and running?
        var compiler = new Compiler();
        var code = compiler.CompileExprForREPL(program, Environment);
        // TODO: if we load a different environment here, doesn't that clobber the old one. And then what?
        // NOTE: also a problem in ExecuteFile
        Runtime.Load(code, Environment, continuation);
        // TODO: move continuation argument to Run?
        Runtime.Run();
    }

    public void Eval(ContinuationAny continuation, Syntax syntax, IRuntimeEnvironment env)
    {
        throw new NotImplementedException();
    }

    public void EvalSequence(IEnumerable<Syntax> syntax, ExpansionContextType type = ExpansionContextType.LibraryBody, ContinuationAny? continuation = null)
    {
        if (continuation is null) {
            continuation = Evaluator.DefaultContinuation;

        }
        var context = new ExpansionContext(this, Environment.TopLevels.Keys, type);
        var parsedProgram =
            Expander.ExpandSequence(syntax, context);
        var compiler = new Compiler();
        ParsedForm[] program = parsedProgram.ToArray();
        var compiled = compiler.CompileFile(program, Environment);
        Runtime.Load(compiled, Environment, continuation);
        Runtime.Run();
    }

    public static void DefaultContinuation(SchemeValue[] results)
    {
        // TODO: this should use the default output port
        // TODO: Evaluator should have a field and an option for passing a continuation to its constructor
        // EValuator factory will also need an optional cstr parameter
        foreach (var val in results) {
            if (val is not SchemeValue.VoidType) {
                // File.AppendAllLines("/home/dave/lan/projects/Jig/log.txt", [form.Print()]);
                Console.WriteLine(val.Print());
            }
        }
    }

    public void Import(ParsedImportForm importForm) {
        foreach (var importSpec in importForm.Specs) {
            if (LibraryLibrary.Instance.TryFindLibrary(importSpec, out ILibrary? library)) {
                ImportKeywords(library, importSpec.Level);
                ImportVariables(library, importSpec.Level);
            } else {
                throw new  Exception($"failed to find library: {importSpec}");
            }
        }
    }

    public void ImportKeywords(ParsedImportForm importForm) {
        foreach (var importSpec in importForm.Specs) {
            if (LibraryLibrary.Instance.TryFindLibrary(importSpec, out ILibrary? library)) {
                ImportKeywords(library, importSpec.Level);
            } else {
                throw new  Exception($"failed to find library: {importSpec}");
            }
        }
    }

    public void ImportKeywords(ILibrary library, int phase = 0) {
        
        IEvaluator evaluator = this;
        while (phase != 0) {
            evaluator = evaluator.Expander.Evaluator;
            phase--;
        }
        foreach (var tup in library.KeywordExports) {
            // TODO: it's so wrong that we are passing around symbols here :(
            evaluator.Keywords.Add(new Identifier(tup.Item1), tup.Item2);
        }
    }

    public void ImportVariables(ParsedImportForm importForm) {
        foreach (var importSpec in importForm.Specs) {
            if (LibraryLibrary.Instance.TryFindLibrary(importSpec, out ILibrary? library)) {
                ImportVariables(library, importSpec.Level);
            } else {
                throw new Exception($"unable to import library: {importSpec}");
            }
        }
    }
    
    public void ImportVariables(ILibrary library, int phase = 0) {
        IEvaluator evaluator = this;
        while (phase != 0) {
            evaluator = evaluator.Expander.Evaluator;
            phase--;
        }
        
        foreach (var binding in library.VariableExports) {
            evaluator.Variables.DefineTopLevel(binding.Parameter, binding);
        }
        
    }

    public void Import(ILibrary library, int phase = 0) {
        IEvaluator evaluator = this;
        while (phase != 0) {
            evaluator = evaluator.Expander.Evaluator;
            phase--;

        }
        
        foreach (var tup in library.KeywordExports) {
            // TODO: it's so wrong that we are passing around symbols here :(
            evaluator.Keywords.Add(new Identifier(tup.Item1), tup.Item2);
        }
        
        foreach (var binding in library.VariableExports) {
            evaluator.Variables.DefineTopLevel(binding.Parameter, binding);
        }

    }

    public Machine Runtime { get; }
    public IRuntimeEnvironment Variables { get; }
    
    public Environment Environment { get; }
}

public class VMFactory : IEvaluatorFactory {

    public VMFactory(uint phase = 0) {
        Phase = phase;
    }

    public uint Phase { get;  }

    public IEvaluator Build() {
        return new Evaluator(Phase);

    }
}
