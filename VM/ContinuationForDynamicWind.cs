namespace VM;

public class ContinuationForDynamicWind : ContinuationForNonTailBody {

    public static ContinuationForDynamicWind FromThunks(
        Procedure inThunk,
        Procedure body,
        Procedure outThunk,
        ulong retAddress,
        Continuation cont,
        Environment envt,
        uint fp, // this is the FP that out-thunk's continuation needs
        Template template) {

        ContForDWBody contForBody = ContForDWBody.FromThunk(outThunk, retAddress, cont, envt, fp, template);
        return new ContinuationForDynamicWind(inThunk, outThunk, body.Template, 0, envt, fp, contForBody);

    }


    public override void Pop(Machine vm) {
        // when inthunk has run in dynamic wind, what needs to be done?
        // 1) save winders.
        vm.Winders.Push(InThunk, OutThunk);
        // Console.WriteLine($"popping continuation for in-thunk. pushed winders to vm. winders length = {vm.Winders.Length}");
        // no need to check how many values were returned, because any are allowed
        // set SP to current FP, erasing all values returned by this call
        vm.SP = vm.FP;
        vm.PC = this.ReturnAddress;
        vm.FP = this.FP; // now set FP to old FP
        vm.ENVT = this.Environment.Extend(0); // we need to extend the environment as part of calling (body-thunk)
        vm.Template = this.Template;
        vm.CONT = this.Continuation;
        
    }

    private ContinuationForDynamicWind(
        Procedure inThunk,
        Procedure outThunk,
        Template template,
        ulong returnAddress,
        Environment environment,
        uint fp,
        Continuation continuation)
        : base(template, returnAddress, environment, fp, continuation) {
        
        InThunk = inThunk;
        OutThunk = outThunk;
        
    }

    public Procedure OutThunk { get; }


    public Procedure InThunk { get; }
    
    public class ContForDWBody : PartialContinuation {
        public static ContForDWBody FromThunk(Procedure outThunk, ulong retAddress, Continuation cont, Environment envt, uint fp, Template template) {


            var contForOut = new ContinuationForNonTailBody(template, retAddress, envt, fp, cont);
            return new ContForDWBody(outThunk.Template, 0, envt, fp, contForOut, 0, true);
        }
        // TODO: does the constructor for ContForBody actually not need fp because it is not used?
        // it's used by ContForDWOut, but not by ContForDWBody
        // TODO: also probably doesn't need requiredValues or hasOptional
        
        private ContForDWBody(
            Template template,
            ulong returnAddress,
            Environment environment,
            uint fp, Continuation continuation,
            int requiredValues,
            bool hasOptional)
            : base(template, returnAddress, environment, fp, continuation, requiredValues, hasOptional) { }

        public override void Pop(Machine vm)
        {
            // Console.WriteLine($"ContForDWBody:Pop! transferring control to:");
            
            // Array.ForEach(Dissassembler.Disassemble(this.Template), Console.WriteLine);
            vm.Winders.Pop();
            vm.PC = this.ReturnAddress;
            vm.FP = vm.SP; // unusually, we are setting the stack frame for the out thunk to the SP.
            // this saves the results of body-thunk on the stack
            vm.ENVT = this.Environment.Extend(0);
            vm.Template = this.Template;
            vm.CONT = this.Continuation;
        }
    }
}