using System.Collections;
using System.Diagnostics.CodeAnalysis;

namespace Jig;

public class Vector : SchemeValue, ISchemeValue, IEnumerable<ISchemeValue> {

    public Vector() {
        Elements = [];
    }

    public Vector(IEnumerable<ISchemeValue> xs)
    {
        Elements = [.. xs];
    }

    public bool TryGetAtIndex(Integer i, [NotNullWhen(returnValue: true)] out ISchemeValue? result) {
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

    protected internal ISchemeValue[] Elements {get;}

    public override string Print() => $"#({string.Join(' ', this.Select(x => x.Print()))})";

    public IEnumerator<ISchemeValue> GetEnumerator() {
        foreach (var x in Elements) {
            yield return x;
        }

    }

    IEnumerator IEnumerable.GetEnumerator() {
        return this.GetEnumerator();
    }

} // class Vector

