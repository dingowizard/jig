using Jig;

namespace VM;

public class SavedContinuation : Continuation {

    public SavedContinuation(Continuation cont, SchemeValue[] savedStack, Winders winders) {
        Required = cont.Required;
        HasOptional = cont.HasOptional;
        Saved = cont;
        SavedStack = savedStack;
        SavedWinders = winders;
    }
    
    
    public Winders SavedWinders { get; }
    
    public override void Pop(Machine machine) {
        int i = 0;
        foreach (var form in SavedStack) {
            machine.Stack[i] = form;
            i++;
        }
        machine.SP = (uint)SavedStack.Length;
        Saved.Pop(machine);
    }

    public void Apply(Machine vm) {

        var results = vm.SaveStackFrameToArray();
        // restore stack
        int i = 0;
        
        
        
        // Console.WriteLine($"\tat this point we ought to have done all the winders if there were any");
        foreach (var form in SavedStack) {
            vm.Stack[i] = form;
            i++;
        }
        // restore stack and frame pointers
        vm.SP = (uint)SavedStack.Length;
        vm.FP = vm.SP;
        
        // push argument to continuation onto stack as result
        foreach (var result in results) {
            vm.Push(result);
        }
        // Console.WriteLine($"We're about to pop the saved cont. the stack is {vm.StackToList().Print()} fp = {vm.FP} sp = {vm.SP}");
        Saved.Pop(vm);
        return;

    }
    
    public Continuation Saved { get; }
    public override int Required { get; }
    public override bool HasOptional { get; }
    
    public SchemeValue[] SavedStack { get; }
}