using Jig;

namespace VM;

public class CompileTimeEnvironment {
    
    private Dictionary<Form.Symbol, Binding> _toplevels;
    public Binding LookUpTopLevel(Form.Symbol sym) {
        if (_toplevels.TryGetValue(sym, out var value)) {
            return value;
        } else
        {
            throw new Exception("syntax error: undeclared variable {sym.Print()}");
        }
    }

    public CompileTimeEnvironment(Environment env) {
        _toplevels = env.TopLevels;
    }

    private CompileTimeEnvironment(CompileTimeEnvironment parent, Frame frame)
    {
        _toplevels = parent._toplevels;
        InnerMostFrame = frame;
    }

    public CompileTimeEnvironment Extend(ParsedLambda.LambdaParameters ps)
    {
        return new CompileTimeEnvironment(this, new Frame(ps, this.InnerMostFrame));
    }
    
    private Frame? InnerMostFrame { get; }

    public (int, int) LookUpLexVar(Form.Symbol sym)
    {
        int frameIndex = 0;
        int slotIndex = 0;
        Frame? frame = InnerMostFrame;
        while (frame is not null) {
            while (slotIndex < frame.Slots.Length) {
                if (frame.Slots[slotIndex].Equals(sym)) return (frameIndex, slotIndex);
                slotIndex++;
            }
            frame = frame.Next;
            frameIndex++;
        }

        throw new Exception("syntax error: use of undeclared lexical variable {sym");
    }

    private class Frame
    {
        public Frame(ParsedLambda.LambdaParameters ps, Frame? parent) {
            Next = parent;
            if (ps.HasRest) {
                var psList = ps.Required.Select(id => id.Symbol).ToList();
                psList.Add(ps.Rest!.Symbol);
                Slots = psList.ToArray();

            } else {
                Slots = ps.Required.Select(id => id.Symbol).ToArray();
            }
        }

        public Form.Symbol[] Slots { get; private set; }
        public Frame? Next { get; }
    }
}