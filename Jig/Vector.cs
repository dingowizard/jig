using System.Collections;
using System.Diagnostics.CodeAnalysis;

namespace Jig;

public class Vector : Form, IForm, IEnumerable<IForm> {
    public Vector(params IForm[] xs) {
        Elements = xs;

    }

    public Vector(List xs)
    {
        Elements = [.. xs];
    }

    public bool TryGetAtIndex(Integer i, [NotNullWhen(returnValue: true)] out IForm? result) {
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

    protected IForm[] Elements {get;}

    public override string Print() => $"#({string.Join(' ', this.Select(x => x.Print()))})";

    public IEnumerator<IForm> GetEnumerator() {
        foreach (var x in Elements) {
            yield return x;
        }

    }

    IEnumerator IEnumerable.GetEnumerator() {
        return this.GetEnumerator();
    }

} // class Vector

