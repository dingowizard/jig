using System.Collections;
using System.Diagnostics.CodeAnalysis;

namespace Jig;

public class Vector : Form, IEnumerable<Form> {
    public Vector(params Form[] xs) {
        Elements = xs;

    }

    public Vector(List xs)
    {
        Elements = [.. xs];
    }

    public bool TryGetAtIndex(Integer i, [NotNullWhen(returnValue: true)] out Form? result) {
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

    protected Form[] Elements {get;}

    public override string Print() => $"#({string.Join(' ', this.Select(x => x.Print()))})";

    public IEnumerator<Form> GetEnumerator() {
        foreach (var x in Elements) {
            yield return x;
        }

    }

    IEnumerator IEnumerable.GetEnumerator() {
        return this.GetEnumerator();
    }

} // class Vector

