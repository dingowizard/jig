using System.Diagnostics.CodeAnalysis;
using EnvEntry = System.Tuple<Jig.Symbol, Jig.SchemeValue, int>;
using Env = System.Collections.Generic.List<System.Tuple<Jig.Symbol, Jig.SchemeValue, int>>;

namespace Jig;

internal static partial class Builtins {

    internal class SyntaxRules {

        [SuppressMessage("ReSharper", "InconsistentNaming")]
        

        internal SyntaxRules(SyntaxList literals, IEnumerable<Tuple<SyntaxList.NonEmpty, Syntax>> clauses) {
            Var = NewSym("x");
            Clauses = clauses.Select<Tuple<SyntaxList.NonEmpty, Syntax>, Clause>(clause => new Clause(Var, literals, clause.Item1, clause.Item2)).ToList();

        }
        


        private Symbol Var {get;}

        private IEnumerable<Clause> Clauses {get;}

        internal List LambdaFromClauses() {
            // makes:
            // (lambda (x) ...)
            // where x will be bound to (syntax-e stx)
            return NewList(
                NewSym("lambda"),
                NewList(Var),
                IfsFromClauses(Var, Clauses));
        }
        private static List IfsFromClauses(Symbol toMatch, IEnumerable<Clause> clauses) {
            var enumerable = clauses as Clause[] ?? clauses.ToArray();
            SchemeValue elseBranch =
            enumerable.Length == 1 ?
            NewList(NewSym("error"), new String($"syntax-rules: couldn't find a match."), toMatch) :
            IfsFromClauses(toMatch, enumerable.Skip(1));

            var thisClause = enumerable[0];
            return NewList(
                NewSym("if"),
                thisClause.Condition(),
                thisClause.ThenBranch(),
                elseBranch
            );
        }

        internal class Clause {
            public Clause(Symbol toMatch, SyntaxList literals, SyntaxList.NonEmpty pattern, Syntax templateSyntax) {

                VarForInput = toMatch;
                Pattern = pattern;
                TemplateSyntax = templateSyntax;
                Literals = literals;
                PatternVars = EnvFromPattern(toMatch, pattern, 0);

            }
            
            private SyntaxList Literals {get;}

            private Env EnvFromPattern(SchemeValue arg, SyntaxList.NonEmpty pattern, int depth)
            {
                if (IsEllipsisPattern(pattern))
                {
                    return EnvFromEllipsisPattern(arg, pattern, depth);
                }
                return EnvFromPattern(
                        NewList( NewSym("car"), arg),
                        pattern.First,
                        depth)
                    .Concat(EnvFromPattern(
                        NewList(NewSym("cdr"), arg),
                        pattern.Rest,
                        depth))
                    .ToList();
            }

            private Env EnvFromPattern(SchemeValue arg, Syntax pattern, int depth)
            {
                if (pattern is Identifier id)
                {
                    return EnvFromPattern(arg, id, depth);
                }

                if (pattern is Syntax.Literal _)
                {
                    return [];
                }

                switch (Syntax.E(pattern))
                {
                    case IEmptyList: return [];
                    case SyntaxList stxList:
                        return EnvFromPattern(
                            NewList(NewSym("syntax-e"), arg),
                            stxList,
                            depth);
                    case SyntaxPair stxPair:
                        return EnvFromPattern(
                            NewList(NewSym("syntax-e"), arg),
                            stxPair,
                            depth);
                    default: throw new Exception($"syntax-rules: unhandled case {pattern}");
                }
                
            }

            private Env EnvFromPattern(SchemeValue arg, SyntaxPair pattern, int depth)
            {
                return EnvFromPattern(
                        NewList( NewSym("car"), arg),
                        pattern.Car,
                        depth)
                    .Concat(EnvFromPattern(
                        NewList(NewSym("cdr"), arg),
                        pattern.Cdr,
                        depth))
                    .ToList();
            }

            private Env EnvFromPattern(SchemeValue arg, SyntaxList pattern, int depth)
            {
                if (pattern is SyntaxList.NonEmpty stxList)
                {
                    return EnvFromPattern(arg, stxList, depth);
                }

                return [];
            }

            private Env EnvFromEllipsisPattern(SchemeValue arg, SyntaxList.NonEmpty pattern, int depth) {
                // TODO: replace map with fold-right
                // ie: (fold-right (lambda (x acc) (cons ((lambda (y) {tup.Item2}) x) acc)) '() {arg})
                var y = NewSym("y");
                Env inner = EnvFromPattern(y, pattern.ElementAt<Syntax>(0), depth + 1);
                return inner.Select(tup =>
                    new EnvEntry(
                        tup.Item1,
                        NewList(
                            NewSym("map"),
                            NewList(
                                NewSym("lambda"),
                                NewList(y),
                                tup.Item2),
                            arg
                        ),
                        tup.Item3))
                    .ToList();
            }

            private Env EnvFromPattern(SchemeValue arg, Identifier id, int depth)
            {
                if (id.Symbol.Name != "_") {
                    return [new EnvEntry(id.Symbol, arg, depth)];
                }

                return [];
            }


            public SchemeValue ThenBranch() {
                    
                    return NewList(
                        NewSym("datum->syntax"),
                        NewSym("stx"),
                        Template(TemplateSyntax, PatternVars));
                    // return Template(TemplateSyntax, EllipsisVars);
            }

            private static SchemeValue Template(Syntax stx, Env env) {
                var env0 = env.FindAll(tup => tup.Item3 == 0);
                if (env0.Count != 0) {
                    var result = NewList(
                        NewList( // ((lambda (p ...) body ...)) ; needs args appended
                            NewSym("lambda"),
                            env0.Select(tup => tup.Item1).ToJigList(), // parameters
                            // body:
                            Template(stx,
                                // decrement any variable with n = 0
                                env.Select(tup => tup.Item3 == 0 ? new EnvEntry(tup.Item1, tup.Item2, tup.Item3 -1) : tup)
                                    .ToList())));
                    return result
                        .Concat(env0
                            .Select(tup => tup.Item2)).ToJigList();
                }
                switch (stx) {
                    case Identifier id:
                        return Template(id, env);
                    case Syntax.Literal lit:
                        return lit;
                }

                if (Syntax.E(stx) is IEmptyList) {
                    return NewList(NewSym("quote"), List.Null);
                }

                if (Syntax.E(stx) is SyntaxList stxList) {
                    return Template(stxList, env);
                }
                if (Syntax.E(stx) is SyntaxPair pair) {
                    return NewList(
                        NewSym("cons"),
                        Template(pair.Car, env),
                        Template(pair.Cdr, env));
                }

                throw new Exception($"syntax-rules: unhandled case in template {stx}");
            }

            private static SchemeValue Template(SyntaxList.NonEmpty stxList, Env env) {
                    if (CadrIsDotDotDot(stxList)) {
                        int numDotDotDots = HowManyDotDotDots(stxList);
                        var rest = stxList.Skip<Syntax>(numDotDotDots + 1).ToSyntaxList();
                        return NewList(
                            NewSym("append"),
                            MapForDotDotDot(stxList.First, NewEnvForDotDotDot(numDotDotDots, env)),
                            Template(rest, env));
                    }
                    return NewList(
                        NewSym("cons"),
                        Template(stxList.First, env),
                        Template(stxList.Rest, env));
            }

            private static Env NewEnvForDotDotDot(int numDotDotDots, Env env) {
                for (; numDotDotDots > 0; numDotDotDots--) {
                    env = env
                        .Select(tup => tup.Item3 > 0
                                                         ? new EnvEntry(
                                                             tup.Item1,
                                                             tup.Item3 > 1
                                                                 ? NewList(NewSym("apply"), NewSym("append"), tup.Item2)
                                                                 : tup.Item2,
                                                             tup.Item3 - 1)
                                                         : tup)
                        .ToList();
                    
                }

                return env;
            }

            private static bool InPattern(Syntax pattern, EnvEntry entry) {
                switch (Syntax.E(pattern)) {
                    case Symbol symbol:
                        return Equals(entry.Item1, symbol) && entry.Item3 == 0;
                    case SyntaxList stxList:
                        return stxList.Any<Syntax>(tup => InPattern(tup, entry));
                    case SyntaxPair pair:
                        return InPattern(pair.Car, entry) || InPattern(pair.Cdr, entry);
                    default: return false;
                        
                }
            }


            private static SchemeValue MapForDotDotDot(Syntax pattern, Env env) {
                var entries = env.FindAll(tup => InPattern(pattern, tup));
                // Console.Error.WriteLine($"entries for {pattern} = {string.Join(", ", entries)}");
                var result = NewList(
                    NewSym("map"),
                    NewList(
                        NewSym("lambda"),
                        entries.Select(tup => tup.Item1).ToJigList(), // params
                        Template(
                            pattern,
                            env.Select(tup => tup.Item3 == 0 ? new EnvEntry(tup.Item1, tup.Item2, -1) : tup)
                                .ToList())));
                
                return result
                    .Concat(
                        entries.Select(tup => tup.Item2))
                    .ToJigList();
            }

            private static int HowManyDotDotDots(SyntaxList.NonEmpty stxList) {
                var array = stxList.Skip<Syntax>(2).ToArray(); // we know the list has at least (a ... )
                int result = 1;
                while (result - 1 < array.Length && array[result - 1] is Identifier { Symbol.Name: "..." }) {
                    result++;
                }

                return result;
            }

            private static SchemeValue Template(SyntaxList stxList, Env env) {
                if (stxList is SyntaxList.NonEmpty nonEmpty) {
                    return Template(nonEmpty, env);
                }

                return NewList(NewSym("quote"), List.Null);
            }

            private static bool CadrIsDotDotDot(SyntaxList stxList) {
                if (stxList.Count<Syntax>() < 2) return false;
                return stxList.ElementAt<Syntax>(1) is Identifier { Symbol.Name: "..." };
            }

            private static SchemeValue Template(Identifier id, Env env) {
                if (env.Any(tup => Equals(tup.Item1, id.Symbol))) {
                    var tup = env.First(tup => Equals(tup.Item1, id.Symbol));
                    if (tup.Item3 > 0) {
                        throw new Exception($"syntax-rules: missing ... in template");
                    }

                    return tup.Item1;
                }
                return NewList(NewSym("quote"), id);

            }


            public SchemeValue Condition() {
                // at the top, we should have an input to match that has been put through syntax-e
                // and the Pattern is a SyntaxList
                // so we don't need to check if it's a pair
                return MakeCondition(VarForInput, VarForInput, Pattern, 0);

            }

            private bool IsEllipsisPattern(SyntaxList pattern) {
                if (pattern.Count<Syntax>() != 2) return false;
                if (pattern.ElementAt<Syntax>(1) is not Identifier id) return false;
                return id.Symbol.Name == "...";
            }

            private SchemeValue MakeEllipsisCondition(SchemeValue stxToMatch, SyntaxList.NonEmpty pattern, int ellipsisDepth)
            {
                // at this point pattern is something like (a ...) where a
                // could be just about anything
                
                var z = NewSym("z");
                var condition = MakeCondition(
                    z,
                    stxToMatch,
                    pattern.ElementAt<Syntax>(0),
                    ellipsisDepth + 1);
                var result = NewList(
                    NewSym("all"),
                    NewList(
                        NewSym("lambda"),
                        NewList(z),
                        condition),
                    stxToMatch );
                return NewList(NewSym("if"), NewList(NewSym("list?"), stxToMatch), result, NewLit(false));

            }

            private SchemeValue MakeCondition(SchemeValue toMatch, SchemeValue forVar, Syntax pattern, int ellipsisDepth = 0)
            {
                if (pattern is Syntax.Literal constant) {
                    return NewList(NewSym("eqv?"), NewList(NewSym("syntax-e"), toMatch), (SchemeValue)constant.Expression);
                    
                }
                switch (Syntax.E(pattern))
                {
                    
                    case IEmptyList: return NewList(NewSym("null?"), NewList(NewSym("syntax-e"), toMatch));
                    case Symbol sym:
                        Identifier? lit = (Identifier?)Literals.FirstOrDefault<Syntax>(stx => stx is Identifier id && id.Symbol.Name == sym.Name);
                        if (lit is not null) {
                            // TODO: probably should test with something other than symbol=?
                            return NewList(
                                NewSym("if"),
                                NewList(NewSym("symbol?"), NewList(NewSym("syntax-e"), toMatch)),
                                NewList(new Symbol("symbol=?"), NewList(NewSym("quote"), sym), NewList(NewSym("syntax-e"), toMatch)),
                                NewLit(false));
                        } 
                        return NewLit(true);
                    case SyntaxList.NonEmpty stxList:
                        // Console.Error.WriteLine($"In {Pattern} \n\tMakeCondition: found list \n\t{stxList} in {toMatch}");
                        return 
                            MakeCondition(
                                NewList(NewSym("syntax-e"), toMatch),
                                NewList(NewSym("syntax-e"), forVar),
                                stxList,
                                ellipsisDepth);
                    case SyntaxPair stxPair:
                        return MakeCondition(
                            NewList(NewSym("syntax-e"), toMatch),
                            NewList(NewSym("syntax-e"), forVar),
                            stxPair,
                            ellipsisDepth);
                    default:
                        throw new NotImplementedException($"SyntaxRules.MakeCondition: unhandled case {pattern}");
                    
                }
            }

            private SchemeValue MakeCondition(SchemeValue toMatch, SchemeValue forVar, SyntaxPair stxPair, int ellipsisDepth) {
                // TODO: if it worked, make this like stxList
                        return NewList(
                            NewSym("if"),
                            NewList(NewSym("pair?"), toMatch),
                            NewList(
                                NewSym("if"),
                                MakeCondition(
                                    NewList(NewSym("car"), toMatch),
                                    NewList(NewSym("car"), forVar),
                                    stxPair.Car,
                                    ellipsisDepth),
                                MakeCondition(
                                    NewList(NewSym("cdr"), toMatch),
                                    NewList(NewSym("cdr"), forVar),
                                    stxPair.Cdr,
                                    ellipsisDepth),
                                NewLit(false)),
                            NewLit(false));

            }


            private SchemeValue MakeCondition(SchemeValue toMatch, SchemeValue forVar, SyntaxList.NonEmpty pattern, int ellipsisDepth) {
                if (IsEllipsisPattern(pattern))
                {
                    return MakeEllipsisCondition(toMatch, pattern, ellipsisDepth);
                }
                return NewList(
                    NewSym("if"),
                    NewList(NewSym("pair?"),  toMatch),
                        NewList(
                            NewSym("if"),
                            MakeCondition(
                                NewList(NewSym("car"), toMatch),
                                NewList(NewSym("car"), forVar),
                                pattern.First,
                                ellipsisDepth),
                            MakeCondition(
                                NewList(NewSym("cdr"), toMatch),
                                NewList(NewSym("cdr"), forVar),
                                pattern.Rest,
                                ellipsisDepth),
                            NewLit(false)),
                    NewLit(false));

            }

            private SchemeValue MakeCondition(SchemeValue toMatch, SchemeValue forVar, SyntaxList pattern, int ellipsisDepth) {
                // This would be called on the cdr if a SyntaxList.NonEmpty
                if (pattern is not SyntaxList.NonEmpty list) return NewList(NewSym("null?"), toMatch);
                // Console.Error.WriteLine($"In {Pattern} \n\tMakeCondition: found list \n\t{list} in {toMatch}");
                return
                    MakeCondition(
                        toMatch,
                        forVar,
                        list,
                        ellipsisDepth);
            }


            private readonly Env  PatternVars;
            private Symbol VarForInput {get;}
            private SyntaxList.NonEmpty Pattern {get;}
            private Syntax TemplateSyntax {get;}
        }

    }


    internal static Symbol NewSym(string name) {
        return new Symbol(name);
    }

    internal static List NewList(params SchemeValue[] forms) {
        return forms.ToJigList();

    }

    internal static Bool NewLit(bool b) {
        return b ? Bool.True : Bool.False;
    }


}
