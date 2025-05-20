using System.Diagnostics;
using Jig;
namespace VM;

public static class WinderThunkCont 
{

    public static Continuation Base(SavedContinuation cont)
    {
        return new BaseCont(cont);
    }

    public static ThunkCont From(Procedure proc, uint fp, Continuation cont)
    {
        // TODO: is fp needed for anything?
        return new ThunkCont(proc, proc.Template, 0, proc.Environment, fp, cont, 0, true);
    }

    public class ThunkCont : PartialContinuation
    {
        public ThunkCont(Procedure thunk, Template template, ulong returnAddress, Environment environment, uint fp, Continuation continuation, int requiredValues, bool hasOptional) : base(template, returnAddress, environment, fp, continuation, requiredValues, hasOptional) {
            Thunk = thunk;
        }
        
        public Procedure Thunk { get; }

        public override void Pop(Machine vm)
        {
            // Console.WriteLine($"Popping continuation for winder");
            // Popping this continuation means transferring control to the winder thunk
            
            // no need to check how many values were returned, because any are allowed
            // set SP to current FP, erasing all values returned by this call
            // Console.WriteLine($"The continuation of this continuation is a {this.Continuation.GetType()}");
            if (this.Continuation is BaseCont b) {
                
                // we need to discard any results returned from the winder
                vm.SP = vm.FP;
                // and the stored FP should be just under the stack pointer
                vm.FP = (uint)((Integer)vm.Pop()).Value;
                vm.Winders = b.SavedContinuation.SavedWinders.Copy();
                vm.VAL = b.SavedContinuation;
                vm.Call();
                return;

            }

            vm.SP = vm.FP; // clear any results from last winder
            vm.VAL = Thunk;
            
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
            // TODO: why is there no code here?

        }

        public override int Required => SavedContinuation.Required;

        public override bool HasOptional => SavedContinuation.HasOptional;
    }

}