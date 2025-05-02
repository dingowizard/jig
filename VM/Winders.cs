using System.Collections;

namespace VM;

public class Winders {

    public Winders Copy()
    {
        return new Winders(_stack);
    }

    public Winders()
    {
        _stack = new Stack<Winder>();
    }

    public override bool Equals(object? obj)
    {
        if (obj == null) return false;
        if (obj is Winders winders)
        {
            var len = winders.Length;
            if (len != Length) return false;
            if (len == 0) return true;
            using var enum1 = winders._stack.GetEnumerator();
            using var enum2 = this._stack.GetEnumerator();

            while (enum1.MoveNext() && enum2.MoveNext())
            {
                if (!EqualityComparer<Winder>.Default.Equals(enum1.Current, enum2.Current))
                    return false;
            }

            return true;

        }

        return false;


    }

    private Winders(Stack<Winder> stack)
    {
        _stack = new Stack<Winder>(new Stack<Winder>(stack));

    }
    public WinderThunkCont.ThunkCont DoWinders(Machine vm, SavedContinuation cont)
    {

        var result = WinderThunkCont.Base(cont);
        
        // in thunks are done after out thunks, so we make the continuations first
        var ws = UnShared(this, vm.Winders);
        // Console.WriteLine($"DoWinders: found {ws.Length} in-thunks ");
        foreach (var w in ws)
        {
            result = WinderThunkCont.From(w.In, vm.FP, result);
        }

        ws = UnShared(vm.Winders, this);
        // Console.WriteLine($"DoWinders: found {ws.Length} out-thunks ");
        for (int i = ws.Length - 1; i >= 0; i--) { 
            result = WinderThunkCont.From(ws[i].Out, vm.FP, result);
        }

        return (WinderThunkCont.ThunkCont)result;
        
    }

    public void Push(Procedure inThunk, Procedure outThunk) {
        _stack.Push(new Winder(inThunk, outThunk));
    }

    public Winder Pop() {
        return _stack.Pop();
    }

    internal int Length => _stack.Count;

    private static Winder[] UnShared(Winders source, Winders other) {
        // Console.WriteLine($"before unshared: source.Length = {source.Length}, other = {other.Length}");
        var ws = source.Copy();
        var xs = other.Copy();
        List<Winder> results = [];
        while (!ws.Equals(xs) && ws.Length > xs.Length) {
            results.Add(ws.Pop());
            // xs.Pop();
        }
        // Console.WriteLine($"after unshared: source.Length = {source.Length}, other = {other.Length}");
        return results.ToArray();

    }

    private Stack<Winder> _stack = new Stack<Winder>();

    public struct Winder(Procedure @in, Procedure @out) {
        public Procedure In { get; } = @in;
        public Procedure Out { get; } = @out;
    }
    
}