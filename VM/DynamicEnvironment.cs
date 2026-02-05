using Jig;
namespace VM;

public abstract class DynamicEnvironment {

    public abstract DynamicEnvironment Copy();

    protected static uint nextID = 0;
    
    protected abstract HashSet<(uint id, SchemeValue val)> Frame {get;}

    public abstract SchemeValue LookUp(uint id);
    
    public abstract void Set(uint id, SchemeValue schemeValue);

    public uint MakeDynamicVariable(SchemeValue schemeValue) {
        Frame.Add((nextID, schemeValue));
        return nextID++;
    }

    public class TopLevel : DynamicEnvironment {

        public TopLevel() {
            // note: it's a little loosey-goosey that we're depending on nextID to be 0 here
            Frame =
            [
                (nextID++, (SchemeValue)Pair.Cons(new Procedure(Environment.Default, Builtins.Abort), List.Null)),

            ];
            // make a dynamic variable for *current-exception-handlers*
            // at slot 0
        }

        public override DynamicEnvironment Copy() {
            return new TopLevel(new HashSet<(uint id, SchemeValue val)>(Frame));
        }

        private TopLevel(HashSet<(uint id, SchemeValue val)> frame) {
            Frame = frame;
        }

        protected override HashSet<(uint id, SchemeValue val)> Frame {get;}
        
        public override SchemeValue LookUp(uint id) {
            (uint _, SchemeValue val) match = Frame.FirstOrDefault(pair => pair.id == id);
            return match.val ?? throw new Exception($"key {id} not found in dynamic environment");
        }
        
        public override void Set(uint id, SchemeValue schemeValue) {
            (uint _, SchemeValue val) match = Frame.FirstOrDefault(pair => pair.id == id);
            if (match.val is null) {
                throw new Exception($"key {id} not found in dynamic environment");
            }
            Frame.Remove(match);
            Frame.Add((id,schemeValue));
        }

    }

    public class Nested : DynamicEnvironment {

        public Nested(DynamicEnvironment parent) {
            Parent = parent;
            Frame = [];
        }

        public override DynamicEnvironment Copy() {
            return new Nested(Parent, new HashSet<(uint id, SchemeValue val)>(Frame));
        }

        private Nested(DynamicEnvironment parent, HashSet<(uint id, SchemeValue val)> frame) {
            Parent = parent;
            Frame = frame;
                
        }
        
        public DynamicEnvironment Parent {get;}

        protected override HashSet<(uint id, SchemeValue val)> Frame {get;}
        
        public override SchemeValue LookUp(uint id) {
            (uint _, SchemeValue val) match = Frame.FirstOrDefault(pair => pair.id == id);
            return match.val ?? Parent.LookUp(id);
        }
        
        public override void Set(uint id, SchemeValue schemeValue) {
            (uint _, SchemeValue val) match = Frame.FirstOrDefault(pair => pair.id == id);
            if (match.val is null) {
                Parent.Set(id, schemeValue);
            } else {
                Frame.Remove(match);
                Frame.Add((id,schemeValue));
            }
        }

    }
    
}