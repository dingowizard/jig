using System.Collections;
using Jig;

namespace VM;

public class Winders {

    public Winders Copy()
    {
        return new Winders(_list);
    }

    public Winders()
    {
        _list = Jig.List.Null;
    }

    public override bool Equals(object? obj)
    {

        if (obj is null) return false;
        if (obj is Winders w)
        {
            return w._list .Equals(_list);
        }

        return false;

    }

    private Winders(Jig.List ws)
    {
        // TODO: hopefully we can remove copying?
        // this seems wrong
        _list = ws;

    }
    public Continuation DoWinders(Machine vm, SavedContinuation cont)
    {
        // TODO: just yuck
        // one good step might be figuring out how to make Winders a JigList rather than having one

        var result = WinderThunkCont.Base(cont);

        List commonTail = CommonTail(this, vm.Winders);
        // in thunks are done after out thunks, so we make the continuations first
        Winders ws = this;
        while (!ws._list.Equals(commonTail))
        {
            // Console.WriteLine($"DoWinders: making continuation for in-thunk");
            List.NonEmpty properWs = (List.NonEmpty)ws._list;
            
            // Array.ForEach(Disassembler.Disassemble(((Winder)properWs.Car).In.Template), Console.WriteLine);
            result = WinderThunkCont.From(((Winder)properWs.Car).In, vm.FP, result, ws);
            ws = new Winders(properWs.Rest);
        }

        ws = vm.Winders;
        int length = ws._list.Length.Value - commonTail.Length.Value;
        // Console.WriteLine($"DoWinders: found {length} out-thunks");
        for (int i = length - 1; i >= 0; i--) { 
            // Array.ForEach(Disassembler.Disassemble(((Winder)ws._list.ElementAt(i)).Out.Template), Console.WriteLine);
            result = WinderThunkCont.From(((Winder)ws._list.ElementAt(i)).Out, vm.FP, result, new Winders(ws._list.Skip(i).ToJigList()));
        }

        return result;
        
    }

    private List CommonTail(Winders winders, Winders vmWinders)
    {
        int lx = winders._list.Length.Value;
        int ly = vmWinders._list.Length.Value;
        List x = lx > ly ? winders._list.Skip(lx - ly).ToJigList() : winders._list;
        List y = ly > lx ? vmWinders._list.Skip(lx - ly).ToJigList() : vmWinders._list;
        while (x is List.NonEmpty propX && y is List.NonEmpty propY && !x.Equals(y))
        {
            x = propX.Rest;
            y = propY.Rest;
            
        }

        return x;

    }

    public void Push(Procedure inThunk, Procedure outThunk) {
        _list = List.Cons(new Winder(inThunk, outThunk), _list);
    }

    public Winder Pop()
    {
        if (_list is List.NonEmpty proper) {
            var result = (Winder)proper.Car;
            _list = proper.Rest;
            return result;
        }
        throw new Exception($"popping winder from empty winders!");
        
    }

    internal int Length => _list.Length.Value;

    private static Winder[] UnShared(Winders source, Winders other) {
        Console.WriteLine($"before unshared: source.Length = {source.Length}, other = {other.Length}");
        var ws = source.Copy();
        var xs = other.Copy();
        System.Collections.Generic.List<Winder> results = [];
        while (!ws.Equals(xs) && ws.Length > xs.Length) {
            results.Add(ws.Pop());
            // xs.Pop();
        }
        // Console.WriteLine($"after unshared: source.Length = {source.Length}, other = {other.Length}");
        return results.ToArray();

    }

    private Jig.List _list = List.Null;

    public class Winder(Procedure @in, Procedure @out) : Form {
        public Procedure In { get; } = @in;
        public Procedure Out { get; } = @out;
        public override string Print()
        {
            throw new NotImplementedException();
        }
    }
    
}