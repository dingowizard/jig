using Jig;

namespace VM;

public abstract class Continuation : Form {
    public override string Print() => "#<continuation>";

    public abstract void Pop(Machine machine);

    public abstract int Required { get; }
    
    public abstract bool HasOptional { get; }
}

public class TopLevelContinuation : Continuation {
    public TopLevelContinuation(ContinuationAny cont) {
        Procedure = cont;
    }

    public override void Pop(Machine vm) {
        var results = new System.Collections.Generic.List<Form>();
        while (vm.SP > 0) {
            results.Add(vm.Pop());
        }

        Procedure(results.ToArray());
    }
    
    public override int Required => 0;
    
    public ContinuationAny Procedure { get; }
    
    public override bool HasOptional => true;

}

public class ContinuationForArg : PartialContinuation {
    public ContinuationForArg(Template template,
        ulong returnAddress,
        Environment env,
        uint fp,
        Continuation cont) : base(template, returnAddress, env, fp, cont, 1, false) {}
}

public class ContinuationForNonTailBody : PartialContinuation {
    public ContinuationForNonTailBody(Template template,
        ulong returnAddress,
        Environment env,
        uint fp,
        Continuation cont) : base(template, returnAddress, env, fp, cont, 0, true) {}
}
public class PartialContinuation : Continuation {

    public override void Pop(Machine vm) {
        
        // check that continuation we are returning to expects the number of values
        if (this.Continuation.HasOptional) {
            if (vm.SP - vm.FP < this.Continuation.Required) {
                throw new Exception(
                    $"continuation expected at least {this.Continuation.Required} values, but received {vm.SP - vm.FP}");
            }
        } else {
            if (vm.SP - vm.FP != this.Continuation.Required) {
                throw new Exception(
                    $"continuation expected {this.Continuation.Required} values, but received {vm.SP - vm.FP}");
            
            }
        }
        vm.PC = this.ReturnAddress;
        vm.FP = this.FP;
        vm.ENVT = this.Environment;
        vm.Template = this.Template;
        vm.CONT = this.Continuation;
    }
    
    public PartialContinuation(Template template,
        ulong returnAddress,
        Environment environment, // TODO: should a continuation have an environment?
        uint fp,
        Continuation continuation,
        int requiredValues,
        bool hasOptional)
    {
        Template = template;
        Continuation = continuation;
        ReturnAddress = returnAddress;
        FP = fp;
        Environment = environment;
        Required = requiredValues;
        HasOptional = hasOptional;
    }

    public Template Template { get; }

    public Continuation Continuation { get; }

    public uint FP;
    public virtual ulong ReturnAddress { get; }
    public override int Required { get; }
    public override bool HasOptional { get; }

    public virtual Environment Environment { get; }
    

}
public class PartialContinuationForCallWithValues : PartialContinuation {
    public PartialContinuationForCallWithValues(Template continuationProcTemplate,
        ulong i,
        Environment envt,
        uint fp,
        Continuation cont,
        int continuationProcRequired,
        bool continuationProcHasRest)
        : base(continuationProcTemplate, i, envt, fp, cont, continuationProcRequired, continuationProcHasRest) {
        
    }

    public override void Pop(Machine vm) {
        vm.ENVT = vm.ENVT.Extend(Template.NumVarsForScope);
        // Console.WriteLine($"Env extended with {Template.NumVarsForScope} slots");
        // check that continuation we are returning to expects the number of values
        if (this.HasOptional) {
            // Console.WriteLine($"continuation expected at least {this.Required} values and received {vm.SP - vm.FP} (SP = {vm.SP} FP = {vm.FP} stack = {vm.StackToList()}");
            if (vm.SP - vm.FP < this.Required) {
                throw new Exception(
                    $"continuation expected at least {this.Required} values, but received {vm.SP - vm.FP}");
            }
        } else {
            // Console.WriteLine($"continuation expected exactly {this.Required} values and received {vm.SP - vm.FP}(SP = {vm.SP} FP = {vm.FP} stack = {vm.StackToList()}");
            if (vm.SP - vm.FP != this.Required) {
                throw new Exception(
                    $"continuation expected exactly {this.Required} values, but received {vm.SP - vm.FP}");
            }
        }
        vm.PC = this.ReturnAddress;
        vm.FP = this.FP;
        vm.Template = this.Template;
        vm.CONT = this.Continuation;
    }
}

public class SavedContinuation : Continuation {

    public SavedContinuation(Continuation cont, Form[] stack) {
        Required = cont.Required;
        HasOptional = cont.HasOptional;
        Saved = cont;
        Stack = stack;
    }
    
    public override void Pop(Machine machine) {
        int i = 0;
        foreach (var form in Stack) {
            machine.Stack[i] = form;
            i++;
        }
        machine.SP = (uint)Stack.Length;
        Saved.Pop(machine);
    }

    public void Apply(Machine vm, Form[] results) {
        int i = 0;
        foreach (var form in Stack) {
            vm.Stack[i] = form;
            i++;
        }
        vm.SP = (uint)Stack.Length;
        vm.FP = vm.SP;
        foreach (var result in results) {
            vm.Push(result);
        }
        Saved.Pop(vm);
        
    }
    
    public Continuation Saved { get; }
    public override int Required { get; }
    public override bool HasOptional { get; }
    
    public Form[] Stack { get; }
}
