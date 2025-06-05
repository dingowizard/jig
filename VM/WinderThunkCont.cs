using System.Diagnostics;
using Jig;
namespace VM;

public static class WinderThunkCont 
{

    public static Continuation Base(SavedContinuation cont)
    {
        return new BaseCont(cont);
    }

    public static ThunkCont From(Procedure proc, uint fp, Continuation cont, Winders ws)
    {
        // TODO: is fp needed for anything?
        return new ThunkCont(proc, proc.Template, 0, proc.Environment, fp, cont, 0, true, ws);
    }

    public class ThunkCont : PartialContinuation
    {
        public ThunkCont(Procedure thunk, Template template, ulong returnAddress, Environment environment, uint fp, Continuation continuation, int requiredValues, bool hasOptional , Winders ws) : base(template, returnAddress, environment, fp, continuation, requiredValues, hasOptional) {
            Thunk = thunk;
            Winders = ws;
        }

        public Winders Winders { get; set; }

        public Procedure Thunk { get; }

        public override void Pop(Machine vm)
        {
            // This represents return from an in- or out-thunk as part of applying a 
            // saved continuation that was made inside dynamic-wind
            
            // no need to check how many values were returned, because any are allowed
            // set SP to current FP, erasing all values returned by this call

            vm.SP = vm.FP;
            vm.PC = this.ReturnAddress;
            vm.ENVT = this.Environment;
            vm.Template = this.Template;
            vm.CONT = this.Continuation;
            
            // TODO: setting winders is supposed to happen _before_ out-thunks
            // this is always setting winders after the thunk whether it is in or out
            vm.Winders = Winders;
            return;
            
        }
    }

    public class BaseCont : Continuation
    {
        public BaseCont(SavedContinuation cont)
        {
            SavedContinuation = cont;
        }

        public SavedContinuation SavedContinuation { get; }

        public override void Pop(Machine vm)
        {
            // After we've done the out- and in-thunks when applying a continuation, this is the saved continuation:
            
            // Console.WriteLine($"BaseCont.Pop:");
            // Array.ForEach(Disassembler.Disassemble(vm.Template), Console.WriteLine);
            // we need to discard any results returned from the last in- or out-thunk
            vm.SP = vm.FP;
            // and the stored FP should be just under the stack pointer
            vm.FP = (uint)((Integer)vm.Pop()).Value;
            vm.Winders = SavedContinuation.SavedWinders.Copy();
            vm.VAL = SavedContinuation;
            vm.Call();
            return;

        }

        public override int Required => SavedContinuation.Required;

        public override bool HasOptional => SavedContinuation.HasOptional;
    }

}