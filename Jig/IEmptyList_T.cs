
namespace Jig;

public interface IEmptyList<T> : IList, IEmptyList, IList<T> where T : ISchemeValue {
}