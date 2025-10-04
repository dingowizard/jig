using Jig;

namespace VM;

public class ContinuationForArg : PartialContinuation {
    public ContinuationForArg(Template template,
        ulong returnAddress,
        Environment2 env,
        Location[] vars,
        uint fp,
        Continuation cont) : base(template, returnAddress, env, vars, fp, cont, 1, false) {}
}

public class ContinuationForNonTailBody : PartialContinuation {
    public ContinuationForNonTailBody(Template template,
        ulong returnAddress,
        Environment2 env,
        Location[] vars,
        uint fp,
        Continuation cont) : base(template, returnAddress, env, vars, fp, cont, 0, true) {}

    public override void Pop(Machine vm) {
        // no need to check how many values were returned, because any are allowed
        // set SP to current FP, erasing all values returned by this call
        vm.SP = vm.FP;
        vm.PC = this.ReturnAddress;
        vm.FP = this.FP; // now set FP to old FP
        vm.ENVT = this.Environment;
        vm.Template = this.Template;
        vm.CONT = this.Continuation;
    }
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
                
                Console.WriteLine($"Error popping PartCont: popping to this template: {this.ReturnAddress}");
                Array.ForEach(Disassembler.Disassemble(this.Template), Console.WriteLine);
                Console.WriteLine($"but checking stack against expected values for {this.Continuation.GetType()}");
                
                throw new Exception(
                    $"continuation expected {this.Continuation.Required} values, but received {vm.SP - vm.FP}. stack = {vm.StackToList().Print()} SP = {vm.SP} FP = {vm.FP}");
            
            }
        }
        vm.PC = this.ReturnAddress;
        vm.FP = this.FP;
        vm.ENVT = this.Environment;
        vm.VARS = this.Variables;
        vm.Template = this.Template;
        vm.CONT = this.Continuation;
    }
    
    public PartialContinuation(
        Template template,
        ulong returnAddress,
        Environment2 environment, // TODO: should a continuation have an environment?
                                 // atm the envt is used by DynamicWind, but it's not clear that it should
                                 // there may be other ways it is used...
        Location[] vars,
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
        Variables = vars;
        Required = requiredValues;
        HasOptional = hasOptional;
    }

    public Template Template { get; }

    public Continuation Continuation { get; }

    public uint FP;

    public virtual ulong ReturnAddress { get; }
    public override int Required { get; }
    public override bool HasOptional { get; }

    public virtual Environment2 Environment { get; protected set; }
    
    public Location[] Variables { get; }
    

}
public class PartialContinuationForCallWithValues : PartialContinuation {
    public PartialContinuationForCallWithValues(Template continuationProcTemplate,
        ulong i,
        Environment2 envt,
        Location[] vars,
        uint fp,
        Continuation cont,
        int continuationProcRequired,
        bool continuationProcHasRest)
        : base(continuationProcTemplate, i, envt, vars, fp, cont, continuationProcRequired, continuationProcHasRest) {
        
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