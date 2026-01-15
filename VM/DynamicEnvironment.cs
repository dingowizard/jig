using Jig;
namespace VM;

public abstract class DynamicEnvironment {

    protected static uint nextID = 0;
    
    protected abstract HashSet<(uint id, SchemeValue val)> Frame {get;}

    public abstract SchemeValue LookUp(uint id);
    
    public abstract void Set(uint id, SchemeValue schemeValue);

    public class TopLevel : DynamicEnvironment {

        public TopLevel() {
            Frame = [];
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