import { assert } from "./util/Core"
import { 
   Parser, ParseResult, ParseState, between, butnot, ch, chainl1, choice, constant, dropFirst,
   dropSecond, seqDep, lexeme, negate, optional, range, repeat, repeat1, satisfying, sepBy1, seq, 
   sequence, symbol, withAction, withJoin
} from "./util/parse/Core"
import { Cons, List, Nil } from "./BaseTypes"
import { arity } from "./DataType"
import { singleton } from "./FiniteMap"
import { ν } from "./Runtime"
import { Expr, Lex, str } from "./Expr"
import { JoinSemilattice } from "./util/Ord";

// General convention: define parsers 'pointfully' (as functions), rather than as combinator expressions,
// whenever the recursive nature of the grammar causes a problem with variable initialisation.
export module Parse {

// ch is a JavaScript "character", i.e. string of length 1. Currently not supporting Unicode
// identifiers.
function isUpper (ch: string) {
   return ch === ch.toUpperCase() && ch !== ch.toLowerCase()
}

// Constructors must start with an uppercase letter, a la Haskell. One advantage of this is that we
// can use the same names (modulo case) for keywords and the constructors of reified terms.
function isCtr (str: string): boolean {
   return isUpper(str.charAt(0))
}

const reservedWord: Parser<string> =
   choice<string>([
      reserved(str.as), reserved(str.match), reserved(str.fun), reserved(str.in_),
      reserved(str.let_), reserved(str.letRec)
   ])

function keyword(str: string): Parser<Lex.Keyword> {
   return lexeme(reserved(str), Lex.Keyword)
}

// No Unicode support for identifiers yet.
const identStart: Parser<string> =
   choice<string>([range("a", "z"), range("A", "Z"), ch("$"), ch("_")])

const identPart: Parser<string> =
   choice<string>([identStart, range("0", "9")])

const identEnd: Parser<string> =
   ch("'")

const identCandidate: Parser<string> =
   withJoin(sequence([identStart, withJoin(repeat(identPart)), withJoin(repeat(identEnd))]))

// Use this to prevent identifiers/keywords that have (other) keywords as prefixes from being
// problematic. Could take a similar approach (defining operatorCandidate) with operators, if we
// wanted Haskell-style operators, where for example >>= and >> must coexist.
// TODO: hoist to Parse module, which will need parameterising on identCandidate.
function reserved (str: string): Parser<string> {
   return (state: ParseState): ParseResult<string> | null => {
      const r: ParseResult<string> | null = identCandidate(state)
      if (r !== null && r.ast === str)
         return r
      return null
   }
}

const ctr: Parser<Lex.Ctr> = 
   lexeme(satisfying(identCandidate, isCtr), Lex.Ctr)

// Note that primitive operations that have names (e.g. intToString) are /exactly/ like regular
// identifiers. They can be shadowed, for example.
const var_: Parser<Lex.Var> =
   lexeme(
      butnot(satisfying(identCandidate, str => !isCtr(str)), reservedWord),
      Lex.Var
   )

const variable: Parser<Expr.Var> =
   withAction(var_, (x: Lex.Var) => Expr.Var.at(ν(), x))

// Only allow Unicode escape sequences (i.e. no hex or octal escapes, nor "character" escapes such as \r).
const hexDigit: Parser<string> = 
   choice<string>([range("0", "9"), range("a", "f"), range("A", "F")])

const unicodeEscape: Parser<string> =
   withAction(
      sequence([ch("x"), hexDigit, hexDigit, hexDigit, hexDigit]),
      chars => String.fromCharCode(parseInt("0x" + chars.join("").substring(1)))
   )

// Standard Java/C escape sequences. They happen to be a subset of JavaScript's escape sequences, so
// this just defines the embedding.
const singleCharEscape: Parser<string> = choice<string>([
   withAction(ch("'"), _ => "'"),
   withAction(ch('"'), _ => '"'),
   withAction(ch("\\"), _ => "\\"),
   withAction(ch("b"), _ => "\b"),
   withAction(ch("f"), _ => "\f"),
   withAction(ch("n"), _ => "\t"),
   withAction(ch("r"), _ => "\r"),
   withAction(ch("t"), _ => "\t")
])

const escapeSeq: Parser<string> =
   dropFirst(ch("\\"), choice<string>([unicodeEscape, singleCharEscape]))

const stringCh: Parser<string> =
   choice<string>([negate(choice<string>([ch('"'), ch("\\"), ch("\r"), ch("\n")])), escapeSeq])

const decimalDigits: Parser<string> = 
   withJoin(repeat1(range("0", "9")))

// To avoid having to deal with arbitrary operator precedence, we classify all operators as one of three
// kinds, depending on the initial character. See 0.5.1 release notes.
const opCandidate: Parser<Lex.OpName> =
   lexeme(
      butnot(
         withJoin(repeat1(choice([ch("+"), ch("*"), ch("/"), ch("-"), ch("="), ch("<"), ch(">")]))),
         symbol(str.equals)
      ),
      Lex.OpName
   )

function isProductOp (opName: Lex.OpName): boolean {
   return opName.str.charAt(0) === "*" || opName.str.charAt(0) === "/"
}

function isSumOp (opName: Lex.OpName): boolean {
   return opName.str.charAt(0) === "+" || opName.str.charAt(0) === "-"
}

const productOp: Parser<Lex.OpName> =
   satisfying(opCandidate, isProductOp)

const sumOp: Parser<Lex.OpName> =
   satisfying(opCandidate, isSumOp)

const compareOp: Parser<Lex.OpName> =
   satisfying(opCandidate, opName => !isProductOp(opName) && !isSumOp(opName))

function parenthesise<T> (p: Parser<T>): Parser<T> {
   return between(symbol(str.parenL), p, symbol(str.parenR))
}

// Consume no input, because application is represented simply by adjacency.
const app_: Parser<(e1: Expr, e2: Expr) => Expr.App> =
   withAction(
      constant(null),
      (_: Expr) =>
         (e1: Expr, e2: Expr): Expr.App => Expr.App.at(ν(), e1, e2)
   )

function appOp (
   opP: Parser<Lex.OpName>
): Parser<(e1: Expr, e2: Expr) => Expr.PrimApp> {
   return withAction(
      opP,
      op =>
         (e1: Expr, e2: Expr): Expr.PrimApp =>
            Expr.PrimApp.at(ν(), e1, op, e2)
   )
}

const string_: Parser<Expr.ConstStr> =
   withAction(
      lexeme(between(ch('"'), withJoin(repeat(stringCh)), ch('"'),), Lex.StringLiteral),
      lit => Expr.ConstStr.at(ν(), lit.str)
   )

const integer: Parser<Expr.ConstInt> =
   withAction(
      lexeme(
         choice<string>([
            decimalDigits,
            withJoin(sequence([ch("+"), decimalDigits])),
            withJoin(sequence([ch("-"), decimalDigits]))
         ]),
         Lex.IntLiteral
      ),
      lit => Expr.ConstInt.at(ν(), parseInt(lit.str))
   )

const parenthExpr: Parser<Expr> = 
   parenthesise(expr)

const let_: Parser<Expr.Let> =
   withAction(
      seq(
         dropFirst(keyword(str.let_), seq(dropSecond(var_, symbol(str.equals)), expr)),
         dropFirst(keyword(str.in_), expr)
      ),
      ([[x, e], eʹ]: [[Lex.Var, Expr], Expr]) =>
         Expr.Let.at(ν(), e, Expr.Trie.Var.make(x, eʹ))
   )

const recDef: Parser<Expr.RecDef> =
   withAction(
      seq(dropFirst(keyword(str.fun), var_), matches),
      ([name, σ]: [Lex.Var, Expr.Trie<Expr>]) =>
         Expr.RecDef.at(ν(), name, Expr.Fun.at(ν(), σ))
   )

const recDefs: Parser<List<Expr.RecDef>> = optional(recDefs1(), Nil.make())

function recDefs1 (): Parser<List<Expr.RecDef>> {
   return (state: ParseState) =>
      withAction(
         seqDep(recDef, (def: Expr.RecDef) => recDefs),
         ([def, δ]: [Expr.RecDef, List<Expr.RecDef>]) => Cons.make(def, δ)
      )(state)
}

const letrec: Parser<Expr.LetRec> =
   withAction(
      seq(
         dropFirst(keyword(str.letRec), recDefs),
         dropFirst(keyword(str.in_), expr)
      ),
     ([δ, body]: [List<Expr.RecDef>, Expr]) => 
         Expr.LetRec.at(ν(), δ, body)
   )

// Enforce consistency with constructor signatures.
const constr: Parser<Expr.Constr> =
   withAction(
      seq(ctr, optional(parenthesise(sepBy1(expr, symbol(","))), [])),
      ([ctr, args]: [Lex.Ctr, Expr[]]) => {
         const n: number = arity(ctr.str)
         assert(n <= args.length, "Too few arguments in constructor.", ctr.str)
         assert(n >= args.length, "Too many arguments in constructor.", ctr.str)
         return Expr.Constr.at(ν(), ctr, List.fromArray(args))
      }
   )

const pair: Parser<Expr.Constr> =
   withAction(
      parenthesise(seq(dropSecond(expr, symbol(",")), expr)),
      ([fst, snd]: [Expr, Expr]) =>
         Expr.Constr.at(ν(), new Lex.Ctr("Pair"), List.fromArray([fst, snd]))
   )

function args_pattern<K extends JoinSemilattice<K>> (n: number, p: Parser<K>): Parser<Expr.Args<K>> {
   if (n === 0) {
      return withAction(p, Expr.Args.End.make)
   } else {
      let pʹ = args_pattern(n - 1, p)
      if (n > 1) {
         pʹ = dropFirst(symbol(","), pʹ)
      }
      return withAction(pattern(pʹ), Expr.Args.Next.make)
   }
}

// Continuation-passing style means "parenthesise" idiom doesn't work here.
function constr_pattern<K extends JoinSemilattice<K>> (p: Parser<K>): Parser<Expr.Trie.Constr<K>> {
   return withAction(
      seqDep(
         ctr, 
         (ctr: Lex.Ctr): Parser<Expr.Args<K>> => {
            const n: number = arity(ctr.str)
            if (n === 0) {
               return withAction(p, Expr.Args.End.make)
            } else {
               return dropFirst(symbol(str.parenL), args_pattern(n, dropFirst(symbol(str.parenR), p)))
            }
         }
      ),
      ([ctr, Π]: [Lex.Ctr, Expr.Args<K>]): Expr.Trie.Constr<K> =>
         Expr.Trie.Constr.make(singleton(ctr.str, Π))
   )
}

function pair_pattern<K extends JoinSemilattice<K>> (p: Parser<K>): Parser<Expr.Trie.Constr<K>> {
   return withAction(
      dropFirst(symbol(str.parenL), args_pattern(2, dropFirst(symbol(str.parenR), p))),
      (Π: Expr.Args<K>): Expr.Trie.Constr<K> => Expr.Trie.Constr.make(singleton("Pair", Π))
   )
}

function variable_pattern<K extends JoinSemilattice<K>> (p: Parser<K>): Parser<Expr.Trie.Var<K>> {
   return withAction(
      seq(var_, p), ([x, κ]: [Lex.Var, K]): Expr.Trie.Var<K> => 
         Expr.Trie.Var.make(x, κ)
      )
}

function pattern<K extends JoinSemilattice<K>> (p: Parser<K>): Parser<Expr.Trie<K>> {
   return (state: ParseState) => 
      choice<Expr.Trie<K>>([variable_pattern(p), pair_pattern(p), constr_pattern(p)])(state)
}

// Chain of singleton tries, followed by an expression.
const match: Parser<Expr.Trie<Expr>> =
   choice<Expr.Trie<Expr>>([
      pattern(dropFirst(symbol(str.arrow), expr)),
      pattern(withAction(matches, (m): Expr => Expr.Fun.at(ν(), m))) // retarded cast
   ])

// Assume at least one match clause.
function matches (state: ParseState): ParseResult<Expr.Trie<Expr>> | null {
   return withAction(
      choice<Expr.Trie<Expr>[]>([
         withAction(match, m => [m]),
         between(symbol("{"), sepBy1(match, symbol(";")), symbol("}"))
      ]),
      (σs: Expr.Trie<Expr>[]) => {
         let σ: Expr.Trie<Expr> = σs[0]
         for (let i = 1; i < σs.length; ++i) {
            σ = Expr.Trie.Trie.join(σ, σs[i])
         } 
         return σ
      }
   )(state)
}

const matchAs: Parser<Expr.MatchAs> =
   withAction(
      seq(
         dropFirst(keyword(str.match), expr),
         dropFirst(keyword(str.as), matches)
      ),
      ([e, σ]: [Expr, Expr.Trie<Expr>]) => Expr.MatchAs.at(ν(), e, σ)
   )

const fun: Parser<Expr.Fun> =
   withAction(
      dropFirst(keyword(str.fun), matches),
      (σ: Expr.Trie<Expr>) => Expr.Fun.at(ν(), σ)
   )

// Any expression other than an operator tree or application chain.
const simpleExpr: Parser<Expr> =
   choice<Expr>([
      variable, string_, integer, parenthExpr, pair, let_, letrec, constr, matchAs, fun
   ])

// A left-associative tree, with applications at the branches, and simple terms at the leaves.
const appChain: Parser<Expr> = chainl1(simpleExpr, app_)

// An expression is an operator tree. An operator tree is a tree whose branches are infix
// binary primitives and whose leaves are application chains. "Sections" currently not supported.
const productExpr: Parser<Expr> = chainl1(appChain, appOp(productOp))
const sumExpr: Parser<Expr> = chainl1(productExpr, appOp(sumOp))
const compareExpr: Parser<Expr> = chainl1(sumExpr, appOp(compareOp))

export function expr (state: ParseState): ParseResult<Expr> | null {
   return compareExpr(state)
}

}
