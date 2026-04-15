using System.Collections;
using System.Diagnostics.CodeAnalysis;
using Jig.Types;

namespace Jig;

public class Vector : SchemeValue, IEnumerable<SchemeValue>, IHasTypeDescriptor {

    public Vector() {
        Elements = [];
    }

    public Vector(IEnumerable<SchemeValue> xs)
    {
        Elements = [.. xs];
    }

    public bool TryGetAtIndex(Integer i, [NotNullWhen(returnValue: true)] out SchemeValue? result) {
        if (i.Value >= Elements.Length) {
            result = null;
            return false;
        } else {
            result = Elements[i.Value];
            return true;
        }

    }

    public Integer Length {
        get {
            return new Integer(Elements.Length);
        }
    }

    public SchemeValue[] Elements {get;}

    public override string Print() => $"#({string.Join(' ', this.Select(x => x.Print()))})";

    public IEnumerator<SchemeValue> GetEnumerator() {
        foreach (var x in Elements) {
            yield return x;
        }

    }

    IEnumerator IEnumerable.GetEnumerator() {
        return this.GetEnumerator();
    }

    public static TypeDescriptor TypeDescriptor {get;} = new SchemeValueTypeDescriptor<Vector>();
} // class Vector

