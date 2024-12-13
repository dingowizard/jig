using System.Diagnostics.CodeAnalysis;
using Microsoft.Scripting.Utils;
using EnvEntry = System.Tuple<Jig.Syntax.Identifier, Jig.Syntax, int>;
using Env = System.Collections.Generic.List<System.Tuple<Jig.Syntax.Identifier, Jig.Syntax, int>>;

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
            Syntax.Identifier stxParam = NewId("stx");
            var result = NewList( // (lambda (stx) ((lambda (x) ...) (syntax-e stx)))
                NewId("lambda"),
                NewList(stxParam),
                NewList(
                    new SyntaxRules(stx, literals.ToSyntaxList(), clauses).LambdaFromClauses(),
                    NewList(NewId("syntax-e"), stxParam)));
            // if (stxList.ElementAt<Syntax>(0) is Syntax.Identifier macroName && macroName.Symbol.Name == "lit") {
                // Console.Error.WriteLine($"syntax-rules expanded to\n\t{result}");
            //}
            return Continuation.ApplyDelegate(k, result);

        }

        private SyntaxRules(Syntax stx, SyntaxList literals, IEnumerable<Tuple<SyntaxList.NonEmpty, Syntax>> clauses) {
            if (literals is not SyntaxList.Empty) {
                throw new NotImplementedException($"syntax-rules: literals ({literals.Print()}) are not supported yet.");
            }
            Literals = literals;
            Var = NewId("x");
            Clauses = clauses.Select(clause => new Clause(Var, clause.Item1, clause.Item2)).ToList();

        }
        

        private SyntaxList Literals {get;}

        private Syntax.Identifier Var {get;}

        private IEnumerable<Clause> Clauses {get;}

        public Syntax LambdaFromClauses() {
            // makes:
            // (lambda (x) ...)
            // where x will be bound to (syntax-e stx)
            return NewList(
                NewId("lambda"),
                NewList(Var),
                IfsFromClauses(Var, Clauses));
        }
        private static Syntax IfsFromClauses(Syntax toMatch, IEnumerable<Clause> clauses) {

        Syntax elseBranch =
            clauses.Count() == 1 ?
            NewList(NewId("error"), NewLit($"syntax-rules: couldn't find a match."), toMatch) :
            IfsFromClauses(toMatch, clauses.Skip(1));

        Clause thisClause = clauses.ElementAt(0);
        return NewList(
            NewId("if"),
            thisClause.Condition(),
            thisClause.ThenBranch(),
            elseBranch
        );
        }

        internal class Clause {
            public Clause(Syntax.Identifier toMatch, SyntaxList.NonEmpty pattern, Syntax templateSyntax) {

                VarForInput = toMatch;
                Pattern = pattern;
                TemplateSyntax = templateSyntax;
                PatternVars = EnvFromPattern(toMatch, pattern, 0);

            }

            private Env EnvFromPattern(Syntax arg, SyntaxList.NonEmpty pattern, int depth)
            {
                if (IsEllipsisPattern(pattern))
                {
                    return EnvFromEllipsisPattern(arg, pattern, depth);
                }
                return EnvFromPattern(
                        NewList( NewId("car"), arg),
                        pattern.First,
                        depth)
                    .Concat(EnvFromPattern(
                        NewList(NewId("cdr"), arg),
                        pattern.Rest,
                        depth))
                    .ToList();
            }

            private Env EnvFromPattern(Syntax arg, Syntax pattern, int depth)
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
                    case SyntaxList stxList:
                        return EnvFromPattern(
                            NewList(NewId("syntax-e"), arg),
                            stxList,
                            depth);
                    case SyntaxPair stxPair:
                        return EnvFromPattern(
                            NewList(NewId("syntax-e"), arg),
                            stxPair,
                            depth);
                    default: throw new Exception("syntax-rules: unhandled case {pattern}");
                }
                
            }

            private Env EnvFromPattern(Syntax arg, SyntaxPair pattern, int depth)
            {
                return EnvFromPattern(
                        NewList( NewId("car"), arg),
                        pattern.Car,
                        depth)
                    .Concat(EnvFromPattern(
                        NewList(NewId("cdr"), arg),
                        pattern.Cdr,
                        depth))
                    .ToList();
            }

            private Env EnvFromPattern(Syntax arg, SyntaxList pattern, int depth)
            {
                if (pattern is SyntaxList.NonEmpty stxList)
                {
                    return EnvFromPattern(arg, stxList, depth);
                }

                return [];
            }

            private Env EnvFromEllipsisPattern(Syntax arg, SyntaxList.NonEmpty pattern, int depth)
            {
                Env inner = EnvFromPattern(NewId("y"), pattern.ElementAt<Syntax>(0), depth + 1);
                return inner.Select(tup =>
                    new EnvEntry(
                        tup.Item1,
                        NewList(
                            NewId("map"),
                            NewList(
                                NewId("lambda"),
                                NewList(NewId("y")),
                                tup.Item2),
                            arg
                        ),
                        tup.Item3))
                    .ToList();
            }

            private Env EnvFromPattern(Syntax arg, Syntax.Identifier id, int depth)
            {
                if (id.Symbol.Name != "_") {
                    return [new EnvEntry(id, arg, depth)];
                }

                return [];
            }


            public Syntax ThenBranch() {
                    
                    return NewList(
                        NewId("datum->syntax"),
                        NewId("stx"),
                        Template(TemplateSyntax, PatternVars));
                    // return Template(TemplateSyntax, EllipsisVars);
            }

            private static Syntax Template(Syntax stx, Env env) {
                var env0 = env.FindAll(tup => tup.Item3 == 0);
                if (env0.Any()) {
                    var result = SyntaxList.FromParams(
                        NewList( // ((lambda (p ...) body ...)) ; needs args appended
                            NewId("lambda"),
                            new Syntax(env0.Select(tup => tup.Item1).ToSyntaxList()), // parameters
                            // body:
                            Template(stx,
                                // decrement any variable with n = 0
                                env.Select(tup => tup.Item3 == 0 ? new EnvEntry(tup.Item1, tup.Item2, tup.Item3 -1) : tup)
                                    .ToList())));
                    return new Syntax(result.Concat(env0.Select(tup => tup.Item2)).ToSyntaxList());
                }
                if (stx is Syntax.Identifier id) {
                    return Template(id, env);
                }
                if (stx is Syntax.Literal lit) {
                    return lit;
                }
                if (Syntax.E(stx) is SyntaxList stxList) {
                    return Template(stxList, env);
                }
                if (Syntax.E(stx) is SyntaxPair pair) {
                    return NewList(
                        NewId("cons"),
                        Template(pair.Car, env),
                        Template(pair.Cdr, env));
                }

                throw new Exception($"syntax-rules: unhandled case in template {stx}");
            }

            private static Syntax Template(SyntaxList.NonEmpty stxList, Env env) {
                    if (CadrIsDotDotDot(stxList)) {
                        int numDotDotDots = HowManyDotDotDots(stxList);
                        var rest = stxList.Skip<Syntax>(numDotDotDots + 1).ToSyntaxList();
                        return NewList(
                            NewId("append"),
                            MapForDotDotDot(stxList.First, NewEnvForDotDotDot(numDotDotDots, env)),
                            Template(rest, env));
                    }
                    return NewList(
                        NewId("cons"),
                        Template(stxList.First, env),
                        Template(stxList.Rest, env));
            }

            private static Env NewEnvForDotDotDot(int numDotDotDots, Env env) {
                for (; numDotDotDots > 0; numDotDotDots--) {
                    var a = env.Find(tup => tup.Item1.Symbol.Name == "a");
                    env = env
                        .Select(tup => tup.Item3 > 0
                                                         ? new EnvEntry(
                                                             tup.Item1,
                                                             tup.Item3 > 1
                                                                 ? NewList(NewId("apply"), NewId("append"), tup.Item2)
                                                                 : tup.Item2,
                                                             tup.Item3 - 1)
                                                         : tup)
                        .ToList();
                    
                }

                return env;
            }

            private static Syntax MapForDotDotDot(Syntax stx, Env env) {
                var result = SyntaxList.FromParams(
                    NewId("map"),
                    NewList(
                        NewId("lambda"),
                        new Syntax(
                            SyntaxList.FromIEnumerable(env
                                .FindAll(tup => tup.Item3 == 0)
                                .Select(tup => new Syntax.Identifier(tup.Item1.Symbol)))), // params
                        Template(
                            stx,
                            env.Select(tup => tup.Item3 == 0 ? new EnvEntry(new Syntax.Identifier(tup.Item1.Symbol), tup.Item2, -1) : tup)
                                .ToList())));
                
                return new Syntax(result
                    .Concat<Syntax>(env
                        .FindAll(tup => tup.Item3 == 0)
                        .Select(tup => Syntax.FromDatum(null, Syntax.ToDatum(tup.Item2))))
                    .ToSyntaxList());
            }

            private static int HowManyDotDotDots(SyntaxList.NonEmpty stxList) {
                var array = stxList.Skip<Syntax>(2).ToArray(); // we know the list has at least (a ... 
                int result = 1;
                while ((result - 1) < array.Length && array[result - 1] is Syntax.Identifier { Symbol.Name: "..." }) {
                    result++;
                }

                return result;
            }

            private static Syntax Template(SyntaxList stxList, Env env) {
                if (stxList is SyntaxList.NonEmpty nonEmpty) {
                    return Template(nonEmpty, env);
                }

                return NewList(NewId("quote"), new Syntax(SyntaxList.Null));
            }

            private static bool CadrIsDotDotDot(SyntaxList stxList) {
                if (stxList.Count<Syntax>() < 2) return false;
                return stxList.ElementAt<Syntax>(1) is Syntax.Identifier { Symbol.Name: "..." };
            }

            private static Syntax Template(Syntax.Identifier id, Env env) {
                if (env.Any(tup => Equals(tup.Item1.Symbol, id.Symbol))) {
                    var tup = env.First(tup => Equals(tup.Item1.Symbol, id.Symbol));
                    if (tup.Item3 > 0) {
                        throw new Exception($"syntax-rules: missing ... in template");
                    }

                    return tup.Item1;
                }

                return NewList(NewId("quote"), id);

            }


            public Syntax Condition() {
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

            private Syntax MakeEllipsisCondition(Syntax stxToMatch, Syntax forVar, SyntaxList pattern, int ellipsisDepth)
            {
                // at this point pattern is something like (a ...) where a
                // could be just about anything
                var z = NewId("z");
                Syntax condition = MakeCondition(
                    z,
                    stxToMatch,
                    pattern.ElementAt<Syntax>(0),
                    ellipsisDepth + 1);
                var result = NewList(
                    NewId("all"),
                    NewList(
                        NewId("lambda"),
                        NewList(new Syntax.Identifier(z.Symbol)),
                        condition),
                    stxToMatch );
                return result;

            }

            private Syntax MakeCondition(Syntax toMatch, Syntax forVar, Syntax pattern, int ellipsisDepth = 0)
            {
                switch (Syntax.E(pattern))
                {
                    case Form.Symbol sym: return NewLit(true);
                    case SyntaxList.NonEmpty stxList:
                        return MakeCondition(
                            NewList(NewId("syntax-e"), toMatch),
                            NewList(NewId("syntax-e"), forVar),
                            stxList,
                            ellipsisDepth);
                    case SyntaxPair stxPair:
                        return MakeCondition(
                            NewList(NewId("syntax-e"), toMatch),
                            NewList(NewId("syntax-e"), forVar),
                            stxPair,
                            ellipsisDepth);
                    default:
                        throw new NotImplementedException();
                    
                }
            }

            private Syntax MakeCondition(Syntax toMatch, Syntax forVar, SyntaxPair stxPair, int ellipsisDepth) {
                        return NewList(
                            NewId("if"),
                            NewList(NewId("pair?"), toMatch),
                            NewList(
                                NewId("if"),
                                MakeCondition(
                                    NewList(NewId("car"), toMatch),
                                    NewList(NewId("car"), forVar),
                                    stxPair.Car,
                                    ellipsisDepth),
                                MakeCondition(
                                    NewList(NewId("cdr"), toMatch),
                                    NewList(NewId("cdr"), forVar),
                                    stxPair.Cdr,
                                    ellipsisDepth),
                                NewLit(false)),
                            NewLit(false));

            }


            private Syntax MakeCondition(Syntax toMatch, Syntax forVar, SyntaxList pattern, int ellipsisDepth) {
                if (IsEllipsisPattern(pattern))
                {
                    return MakeEllipsisCondition(toMatch, forVar, pattern, ellipsisDepth);
                }
                switch (pattern) {
                    case SyntaxList.Empty: return NewList(NewId("null?"), toMatch);
                    case SyntaxList.NonEmpty list:
                        return NewList(
                            NewId("if"),
                            NewList(NewId("pair?"), toMatch),
                            NewList(
                                NewId("if"),
                                MakeCondition(
                                    NewList(NewId("car"), toMatch),
                                    NewList(NewId("car"), forVar),
                                    list.First,
                                    ellipsisDepth),
                                MakeCondition(
                                    NewList(NewId("cdr"), toMatch),
                                    NewList(NewId("cdr"), forVar),
                                    list.Rest,
                                    ellipsisDepth),
                                NewLit(false)),
                            NewLit(false));
                    default: throw new Exception("should be impossible. a list is empty or not");
                }

            }


            private Env  PatternVars = [];
            public Syntax.Identifier VarForInput {get;}
            public SyntaxList.NonEmpty Pattern {get;}
            public Syntax TemplateSyntax {get;}
        }

    }


    internal static Syntax.Identifier NewId(string name, SrcLoc? srcLoc = null) {
        
        var result = new Syntax.Identifier(new Form.Symbol(name), srcLoc);

        return result;
    }

    internal static Syntax NewList(params Syntax[] stxs) {
        return new Syntax(stxs.ToSyntaxList());

    }

    internal static Syntax.Literal NewLit(string str) {
        return new Syntax.Literal(new String(str));
    }

    internal static Syntax.Literal NewLit(bool b) {
        return b ? new Syntax.Literal(Bool.True) : new Syntax.Literal(Bool.False);
    }


}
