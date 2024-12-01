using System.Diagnostics.CodeAnalysis;
using Microsoft.Scripting.Utils;

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
            return Continuation.ApplyDelegate(k, result);

        }

        private SyntaxRules(Syntax stx, SyntaxList literals, IEnumerable<Tuple<SyntaxList.NonEmpty, Syntax>> clauses) {
            STX = stx;
            if (literals is not SyntaxList.Empty) {
                throw new NotImplementedException($"syntax-rules: literals ({literals.Print()}) are not supported yet.");
            }
            Literals = literals;
            Var = NewId("x");
            Clauses = clauses.Select(clause => new Clause(Var, clause.Item1, clause.Item2)).ToList();

        }
        
        private Syntax STX { get; }

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
            public Clause(Syntax.Identifier toMatch, SyntaxList.NonEmpty pattern, Syntax template) {

                VarForInput = toMatch;
                Pattern = pattern;
                Template = template;

            }

            public Syntax ThenBranch() {
                    return ThenExprFromTemplate(Template);
            }

            private Syntax ThenExprFromTemplate(Syntax template)
            {
                // make this (lambda (ps ...)) as SyntaxList
                var lambdaExpr = SyntaxList.FromParams(
                    NewId("lambda"),
                    new Syntax(EllipsisVars.Select(tup => tup.Item1).ToSyntaxList()),
                    BodyFromTemplate(template));
                // now ((lambda (ps* ...) bodies ...+) args* ...)
                return new Syntax(
                    SyntaxList.Cons(
                        new Syntax(lambdaExpr),
                        EllipsisVars.Select(tup => tup.Item2).ToSyntaxList()));

            }

            private Syntax BodyFromTemplate(Syntax template)
            {
                var body = Syntax.E(template) switch
                {
                    SyntaxList xs => SyntaxList.FromParams(NewId("quasiquote"), Quasiquoted(xs)),
                    LiteralExpr lit => SyntaxList.FromParams(NewId("quote"), template),
                    _ => throw new NotImplementedException()
                };
                return 
                    NewList(
                        NewId("datum->syntax"),
                        NewId("stx"),
                        new Syntax(body));
            }

            private Syntax Quasiquoted(SyntaxList xs)
            {
                return new Syntax(xs.Select<Syntax, Syntax>(stx => Quasiquoted(stx)).ToSyntaxList());
            }

            private Syntax Quasiquoted(Syntax x)
            {
                if (Syntax.E(x) is SyntaxList xs)
                {
                    return Quasiquoted(xs);
                }
                switch (x)
                {
                    case Syntax.Identifier id:
                        if (EllipsisVars.Find(tup => tup.Item1.Symbol.Equals(id.Symbol)) is { } t)
                        {
                            return NewList(NewId("unquote"), t.Item1);
                        }
                        return id;
                    case Syntax.Literal:
                        return x;
                    default:
                        throw new NotImplementedException(x.Print());
                }
            }

            public Syntax Condition() {
                // at the top, we should have an input to match that has been put through syntax-e
                // and the Pattern is a SyntaxList
                // so we don't need to check if it's a pair
                if (IsEllipsisPattern(Pattern))
                {
                    return MakeEllipsisCondition(VarForInput, Pattern, 0);
                }
                return NewList(
                    NewId("if"),
                    MakeCondition(
                        NewList(NewId("car"), VarForInput),
                        Pattern.First),
                    MakeCondition(
                        NewList(NewId("cdr"), VarForInput),
                        Pattern.Rest,
                        0),
                    NewLit(false));

            }

            private bool IsEllipsisPattern(SyntaxList pattern) {
                if (pattern.Count<Syntax>() != 2) return false;
                if (pattern.ElementAt<Syntax>(1) is not Syntax.Identifier id) return false;
                return id.Symbol.Name == "...";
            }

            private Syntax MakeEllipsisCondition(Syntax stxToMatch, SyntaxList pattern, int ellipsisDepth)
            {
                // at this point pattern is something like (a ...) where a
                // could be just about anything
                var x = NewId("z");
                Syntax condition = MakeCondition(stxToMatch, pattern.ElementAt<Syntax>(0), ellipsisDepth + 1);
                var result = NewList(
                    NewId("all"),
                    NewList(
                        NewId("lambda"),
                        NewList(x),
                        condition),
                    stxToMatch );
                Console.WriteLine($"MakeEllipsisCondition: test for {pattern} = {result.Print()}");
                return result;

            }

            private Syntax MakeCondition(Syntax toMatch, Syntax pattern, int ellipsisDepth = 0)
            {
                switch (Syntax.E(pattern))
                {
                    case Form.Symbol sym:
                        Syntax.Identifier id = (Syntax.Identifier)pattern;
                        if (sym.Name != "_") {
                            EllipsisVars.Add(
                                new Tuple<Syntax.Identifier, Syntax, int>(
                                    id,
                                    toMatch,
                                    ellipsisDepth));
                        }
                        return NewLit(true);
                    case SyntaxList.NonEmpty stxList:
                        return MakeCondition(NewList(NewId("syntax-e"), toMatch), stxList, ellipsisDepth);
                    case SyntaxPair stxPair:
                        return MakeCondition(NewList(NewId("syntax-e"), toMatch), stxPair);
                    default:
                        throw new NotImplementedException();
                    
                }
            }

            private Syntax MakeCondition(Syntax toMatch, SyntaxPair stxPair) {
                        return NewList(
                            NewId("if"),
                            NewList(NewId("pair?"), toMatch),
                            NewList(
                                NewId("if"),
                                MakeCondition(
                                    NewList(NewId("car"), toMatch),
                                    stxPair.Car),
                                MakeCondition(
                                    NewList(NewId("cdr"), toMatch),
                                    stxPair.Cdr),
                                NewLit(false)),
                            NewLit(false));

            }


            private Syntax MakeCondition(Syntax toMatch, SyntaxList pattern, int ellipsisDepth) {
                if (IsEllipsisPattern(pattern))
                {
                    return MakeEllipsisCondition(toMatch, pattern, ellipsisDepth);
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
                                    list.First,
                                    ellipsisDepth),
                                MakeCondition(
                                    NewList(NewId("cdr"), toMatch),
                                    list.Rest,
                                    ellipsisDepth),
                                NewLit(false)),
                            NewLit(false));
                    default: throw new Exception("should be impossible. a list is empty or not");
                }

            }


            private System.Collections.Generic.List<Tuple<Syntax.Identifier, Syntax, int>> EllipsisVars =
                new System.Collections.Generic.List<Tuple<Syntax.Identifier, Syntax, int>>();
            public Syntax.Identifier VarForInput {get;}
            public SyntaxList.NonEmpty Pattern {get;}
            public Syntax Template {get;}
        }

    }


    internal static Syntax.Identifier NewId(string name, SrcLoc? srcLoc = null) {
        return new Syntax.Identifier(new Form.Symbol(name), srcLoc);
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
