using System.Diagnostics.CodeAnalysis;
using EnvEntry = System.Tuple<Jig.Symbol, Jig.Form, int>;
using Env = System.Collections.Generic.List<System.Tuple<Jig.Symbol, Jig.Form, int>>;

namespace Jig;

internal static partial class Builtins {

    internal class SyntaxRules {

        [SuppressMessage("ReSharper", "InconsistentNaming")]
        internal static Thunk? macro(Delegate k, Form arg) {
            if (arg is not Syntax stx) {
                throw new Exception($"syntax-rules: was passed {arg.Print()}, which is not a syntax object");
            }
            if (Syntax.E(stx) is not SyntaxList.NonEmpty stxList) {
                throw new Exception($"syntax-rules: was passed {stx.Print()}, which is not a syntax list");
            }
            if (stxList.Rest is not SyntaxList.NonEmpty macroArgs) {
                throw new Exception($"syntax-rules: expected subforms, but got none");
            }
            if (Syntax.E(macroArgs.First) is not SyntaxList literals) {
                if (Syntax.E(macroArgs.First) is not IEmptyList) {
                    throw new Exception($"syntax-rules: expected first subform to be a list, but got {Syntax.E(macroArgs.First).Print()}");
                }
                literals = SyntaxList.FromParams();

            }
            var clauses = new System.Collections.Generic.List<Tuple<SyntaxList.NonEmpty, Syntax>>();
            foreach (var clause in macroArgs.Rest.Cast<Syntax>()) {
                if (Syntax.E(clause) is SyntaxList.NonEmpty clauseStxList) {
                    if (Syntax.E(clauseStxList.First) is not SyntaxList.NonEmpty pattern) {
                        throw new Exception($"syntax-rules: malformed clause: {clause.Print()}. Pattern should be a list, but got {clauseStxList.First}");
                    }
                    if (clauseStxList.Rest is not SyntaxList.NonEmpty rest) {
                        throw new Exception($"syntax-rules: malformed clause: {clause.Print()}. Expected template.");
                    }
                    if (rest.Rest is not SyntaxList.Empty) {
                        throw new Exception($"syntax-rules: malformed clause: {clause.Print()}. Expected one template.");
                    }
                    clauses.Add(new Tuple<SyntaxList.NonEmpty,Syntax>(pattern, rest.First));

                } else {
                    throw new Exception($"syntax-rules: malformed clause: {clause.Print()}");
                }
            }
            var stxParam = NewSym("stx");
            var result = NewList( // (lambda (stx) ((lambda (x) ...) (syntax-e stx)))
                NewSym("lambda"),
                NewList(stxParam),
                NewList(
                    new SyntaxRules(literals.ToSyntaxList(), clauses).LambdaFromClauses(),
                    NewList(NewSym("syntax-e"), stxParam)));
            return Continuation.ApplyDelegate(k, Syntax.FromDatum(stx.SrcLoc, result));

        }

        private SyntaxRules(SyntaxList literals, IEnumerable<Tuple<SyntaxList.NonEmpty, Syntax>> clauses) {
            Var = NewSym("x");
            Clauses = clauses.Select<Tuple<SyntaxList.NonEmpty, Syntax>, Clause>(clause => new Clause(Var, literals, clause.Item1, clause.Item2)).ToList();

        }
        


        private Symbol Var {get;}

        private IEnumerable<Clause> Clauses {get;}

        private List LambdaFromClauses() {
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
            Form elseBranch =
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

            private Env EnvFromPattern(Form arg, SyntaxList.NonEmpty pattern, int depth)
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

            private Env EnvFromPattern(Form arg, Syntax pattern, int depth)
            {
                if (pattern is Syntax.Identifier id)
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

            private Env EnvFromPattern(Form arg, SyntaxPair pattern, int depth)
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

            private Env EnvFromPattern(Form arg, SyntaxList pattern, int depth)
            {
                if (pattern is SyntaxList.NonEmpty stxList)
                {
                    return EnvFromPattern(arg, stxList, depth);
                }

                return [];
            }

            private Env EnvFromEllipsisPattern(Form arg, SyntaxList.NonEmpty pattern, int depth) {
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

            private Env EnvFromPattern(Form arg, Syntax.Identifier id, int depth)
            {
                if (id.Symbol.Name != "_") {
                    return [new EnvEntry(id.Symbol, arg, depth)];
                }

                return [];
            }


            public Form ThenBranch() {
                    
                    return NewList(
                        NewSym("datum->syntax"),
                        NewSym("stx"),
                        Template(TemplateSyntax, PatternVars));
                    // return Template(TemplateSyntax, EllipsisVars);
            }

            private static Form Template(Syntax stx, Env env) {
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
                    case Syntax.Identifier id:
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

            private static Form Template(SyntaxList.NonEmpty stxList, Env env) {
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


            private static Form MapForDotDotDot(Syntax pattern, Env env) {
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
                while (result - 1 < array.Length && array[result - 1] is Syntax.Identifier { Symbol.Name: "..." }) {
                    result++;
                }

                return result;
            }

            private static Form Template(SyntaxList stxList, Env env) {
                if (stxList is SyntaxList.NonEmpty nonEmpty) {
                    return Template(nonEmpty, env);
                }

                return NewList(NewSym("quote"), List.Null);
            }

            private static bool CadrIsDotDotDot(SyntaxList stxList) {
                if (stxList.Count<Syntax>() < 2) return false;
                return stxList.ElementAt<Syntax>(1) is Syntax.Identifier { Symbol.Name: "..." };
            }

            private static Form Template(Syntax.Identifier id, Env env) {
                if (env.Any(tup => Equals(tup.Item1, id.Symbol))) {
                    var tup = env.First(tup => Equals(tup.Item1, id.Symbol));
                    if (tup.Item3 > 0) {
                        throw new Exception($"syntax-rules: missing ... in template");
                    }

                    return tup.Item1;
                }
                return NewList(NewSym("quote"), id);

            }


            public Form Condition() {
                // at the top, we should have an input to match that has been put through syntax-e
                // and the Pattern is a SyntaxList
                // so we don't need to check if it's a pair
                return MakeCondition(VarForInput, VarForInput, Pattern, 0);

            }

            private bool IsEllipsisPattern(SyntaxList pattern) {
                if (pattern.Count<Syntax>() != 2) return false;
                if (pattern.ElementAt<Syntax>(1) is not Syntax.Identifier id) return false;
                return id.Symbol.Name == "...";
            }

            private Form MakeEllipsisCondition(Form stxToMatch, SyntaxList.NonEmpty pattern, int ellipsisDepth)
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

            private Form MakeCondition(Form toMatch, Form forVar, Syntax pattern, int ellipsisDepth = 0)
            {
                if (pattern is Syntax.Literal constant) {
                    return NewList(NewSym("eqv?"), NewList(NewSym("syntax-e"), toMatch), (Form)constant.Expression);
                    
                }
                switch (Syntax.E(pattern))
                {
                    
                    case IEmptyList: return NewList(NewSym("null?"), NewList(NewSym("syntax-e"), toMatch));
                    case Symbol sym:
                        Syntax.Identifier? lit = (Syntax.Identifier?)Literals.FirstOrDefault<Syntax>(stx => stx is Syntax.Identifier id && id.Symbol.Name == sym.Name);
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

            private Form MakeCondition(Form toMatch, Form forVar, SyntaxPair stxPair, int ellipsisDepth) {
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


            private Form MakeCondition(Form toMatch, Form forVar, SyntaxList.NonEmpty pattern, int ellipsisDepth) {
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

            private Form MakeCondition(Form toMatch, Form forVar, SyntaxList pattern, int ellipsisDepth) {
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


    private static Symbol NewSym(string name) {
        return new Symbol(name);
    }

    private static List NewList(params Form[] forms) {
        return forms.ToJigList();

    }

    private static Bool NewLit(bool b) {
        return b ? Bool.True : Bool.False;
    }


}
