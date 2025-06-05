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
            
            // no need to check how many values were returned, because any are allowed
            // set SP to current FP, erasing all values returned by this call

            // Console.WriteLine($"ThunkCont.Pop");
            // Console.WriteLine($"ThunkCont.Pop: current template:");
            // Array.ForEach(Disassembler.Disassemble(vm.Template), Console.WriteLine);
            vm.SP = vm.FP; // clear any results from last winder
            vm.PC = this.ReturnAddress;
            vm.ENVT = this.Environment;
            vm.Template = this.Template;
            vm.CONT = this.Continuation;
            
            // Console.WriteLine($"ThunkCont.Pop: new template:");
            // Array.ForEach(Disassembler.Disassemble(vm.Template), Console.WriteLine);
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
            // Console.WriteLine($"BaseCont.Pop:");
            // Array.ForEach(Disassembler.Disassemble(vm.Template), Console.WriteLine);
            // we need to discard any results returned from the winder
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