using Jig;

namespace VM;

public class Environment {
    
    private Dictionary<Jig.Form.Symbol, Binding> _topLevelDict = new();

    private Environment(Environment parent, Form[] forms) {
        _topLevelDict = parent._topLevelDict;
        InnerMostFrame = new Frame(forms, parent.InnerMostFrame);
    }

    public Frame InnerMostFrame { get; private set; }

    public Environment Extend(Jig.List forms) {
        // TODO: shouldn't we get rid of IForm??
        return new Environment(this, forms.Cast<Form>().ToArray());
    } 

    public Form LookUpLexVar(int frameIndex, int slot) {
        Frame frame = InnerMostFrame;
        while (frameIndex != 0) {
            if (frame.Next is not null) {
                frame = frame.Next;
                frameIndex--;
                continue;
            }
            else throw new Exception($"Environment.LookUpLexVar: frames not deep enough.");
        }

        if (frame.Slots.Length <= slot) {
            throw new Exception($"Environment.LookUpLexVar: slot index too long for frame.");
        }

        return frame.Slots[slot];
    }

    public class Frame {
        public Frame(Form[] forms, Frame parentInnerMostFrame) {
            Slots = forms;
            Next = parentInnerMostFrame;
        }

        public Form[] Slots { get; private set; }
        public Frame? Next { get; }

        public void Add(ulong slot, Form form) {
            // TODO: make sure slot is not bigger than int
            if (Slots.Length != (int)slot) {
                throw new Exception($"error adding local {form} to frame: wrong index {slot}");
            }
            var newSlots = Slots.ToList();
            newSlots.Add(form);
            Slots = newSlots.ToArray();
        }
    }

    public Environment ExtendWRest(List evalStack, ulong requiredNo) {
        Form[] frame = new Form[requiredNo + 1];
        List forms = evalStack;
        for (ulong i = 0; i < requiredNo; i++) {
            if (forms is Jig.List.NonEmpty proper) {
                frame[i] = (Form)proper.Car;
            } else {
                throw new Exception($"Environment.ExtendWRest: ran out of args!");
            }

            forms = proper.Rest;

        }

        frame[requiredNo] = forms;
        return new Environment(this, frame);

    }

    public void DefineLocal(ulong slot, Form form) {
        InnerMostFrame.Add(slot, form);
    }

    public void SetLexVar(int frameIndex, int slot, Form form) {
        Frame frame = InnerMostFrame;
        while (frameIndex != 0) {
            if (frame.Next is not null) {
                frame = frame.Next;
                frameIndex--;
                continue;
            }
            else throw new Exception($"Environment.SetLexVar: frames not deep enough.");
        }

        if (frame.Slots.Length <= slot) {
            throw new Exception($"Environment.SetLexVar: slot index too long for frame.");
        }

        frame.Slots[slot] = form;
    }
}

