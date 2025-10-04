using Jig;
using Jig.Expansion;

namespace VM;

public class Evaluator : IEvaluator<Machine> {

    public Evaluator(uint phase = 0) {
        Phase =  phase;
        Expander = new Expander(this, new VMFactory(Phase + 1));
        Variables = Environment = Environment2.Minimal();
        Runtime = new Machine(this, (Environment2)Variables);
        Keywords = Runtime.FreshCoreSyntax();

    }

    public uint Phase { get;  }

    public IExpansionRule EvaluateTransformerExpression(ParsedLambda transformerLambdaExpr, ExpansionContext context)
    {
        
        // TODO: needs a compiler, a compile-time environment
        if (Environment is null) throw new Exception($"unable to evaluate transformer expression: Environment was null.");
        if (Environment.TopLevels.Keys.Count == 0)
        {
            throw new Exception($"unable to evaluate transformer expression: Environment {Environment.GetHashCode()} is empty");
        }
        var compiler = new VM.Compiler(); // should class be static?
        var code = compiler.CompileExprForREPL(transformerLambdaExpr, Environment);
        ISchemeValue result = List.Null;
        Runtime.Load(code, Environment, Cont);
        Runtime.Run();
        Procedure proc = result as Procedure ?? throw new Exception("a transformer shold evaluate to a procedure");
        return new Transformer(proc, Runtime);
        
        void Cont(SchemeValue[] forms) => result = forms[0];
    }
    
    
    // public Syntax ApplyTransformer(Jig.Expansion.Transformer transformer, Syntax syntax) {}
    IRuntime IEvaluator.Runtime => Runtime;

    public Expander Expander { get; }
    public IEvaluatorFactory Factory { get; } = new VMFactory();

    public SyntaxEnvironment Keywords { get; }

    public void Eval(ContinuationAny continuation, Syntax syntax)
    {
        var context = new ExpansionContext(this, Environment.TopLevels.Keys);
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

    public void EvalSequence(ContinuationAny continuation, IEnumerable<Syntax> syntax)
    {
        var parsedProgram = Expander.ExpandFile(syntax, new ExpansionContext(this, Environment.TopLevels.Keys));
        var compiler = new Compiler();
        var compiled = compiler.CompileFile(parsedProgram.ToArray(), Environment);
        Runtime.Load(compiled, Environment, continuation);
        Runtime.Run();
    }

    public void Import(ILibrary library, uint phase = 0)
    {
        IEvaluator evaluator = this;
        while (phase != 0)
        {
            evaluator = evaluator.Expander.Evaluator;
            phase--;

        }
        foreach (var binding in library.VariableExports) {
            evaluator.Variables.DefineTopLevel(binding.Symbol, binding);
        }

        foreach (var tup in library.KeywordExports)
        {
            // TODO: it's so wrong that we are passing around symbols here :(
            evaluator.Keywords.Add(new Identifier(tup.Item1), tup.Item2);
        }
    }

    public Machine Runtime { get; }
    public IRuntimeEnvironment Variables { get; }
    
    public Environment2 Environment { get; }
}

public class VMFactory : IEvaluatorFactory {

    public VMFactory(uint phase = 0)
    {
        Phase = phase;
    }

    public uint Phase { get;  }

    public IEvaluator Build()
    {
        return new Evaluator(Phase);

    }
}
