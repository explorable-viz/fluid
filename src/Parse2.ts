import { assert } from "./util/Core"
import { 
   Parser, ParseResult, ParseState, between, butnot, ch, chainl1, choice, constant, dropFirst,
   dropSecond, seqDep, lexeme_, negate, optional, range, repeat, repeat1, satisfying, sepBy1, seq, 
   sequence, symbol, withAction, withJoin
} from "./util/parse/Core2"
import { Cons, List, Nil, Pair, nil } from "./BaseTypes2"
import { arity } from "./DataType2"
import { Expr, Kont, strings } from "./Expr2"
import { singleton } from "./FiniteMap2"
import { Str, num, str } from "./Value2"
import { ν } from "./Versioned2"

import App = Expr.App
import Args = Expr.Args
import BinaryApp = Expr.BinaryApp
import ConstNum = Expr.ConstNum
import ConstStr = Expr.ConstStr
import Constr = Expr.Constr
import Def = Expr.Def
import Defs = Expr.Defs
import Fun = Expr.Fun
import Let2 = Expr.Let2
import LetRec2 = Expr.LetRec2
import MatchAs = Expr.MatchAs
import RecDef = Expr.RecDef
import Trie = Expr.Trie
import Var = Expr.Var

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
      reserved(strings.as), reserved(strings.match), reserved(strings.fun), reserved(strings.in_),
      reserved(strings.let_), reserved(strings.letRec)
   ])

function keyword (str: string): Parser<string> {
   return lexeme_(reserved(str))
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

const ctr: Parser<Str> =
   withAction(lexeme_(satisfying(identCandidate, isCtr)), str)

// Note that primitive operations that have names (e.g. intToString) are /exactly/ like regular
// identifiers. They can be shadowed, for example.
const var_: Parser<Str> =
   withAction(
      lexeme_(butnot(satisfying(identCandidate, c => !isCtr(c)), reservedWord)),
      str
   )

const variable: Parser<Var> =
   withAction(var_, (x: Str) => Expr.var_(ν(), x))

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

// To avoid having to deal with arbitrary operator precedence, we classify all operators as one of three
// kinds, depending on the initial character. See 0.5.1 release notes.
const opCandidate: Parser<string> =
   lexeme_(
      butnot(
         withJoin(repeat1(choice([ch("+"), ch("*"), ch("/"), ch("-"), ch("="), ch("<"), ch(">")]))),
         symbol(strings.equals)
      )
   )

// TODO: consolidate with Primitive.ts
function isProductOp (opName: string): boolean {
   return opName === "*" || opName === "/"
}

function isSumOp (opName: string): boolean {
   return opName === "+" || opName === "-" || opName === "++"
}

function isCompareOp (opName: string): boolean {
   return opName === "==" || opName === "===" || 
          opName === "<" || opName === "<<" || opName === ">" || opName === ">>"
}

const productOp: Parser<Str> =
   withAction(satisfying(opCandidate, isProductOp), str)

const sumOp: Parser<Str> =
   withAction(satisfying(opCandidate, isSumOp), str)

const compareOp: Parser<Str> =
   withAction(satisfying(opCandidate, isCompareOp), str)

function parenthesise<T> (p: Parser<T>): Parser<T> {
   return between(symbol(strings.parenL), p, symbol(strings.parenR))
}

// Consume no input, because application is represented simply by adjacency.
const app_: Parser<(e1: Expr, e2: Expr) => App> =
   withAction(
      constant(null),
      (_: Expr) =>
         (e1: Expr, e2: Expr): App => Expr.app(ν(), e1, e2)
   )

function appOp (opP: Parser<Str>): Parser<(e1: Expr, e2: Expr) => BinaryApp> {
   return withAction(
      opP,
      op =>
         (e1: Expr, e2: Expr): BinaryApp =>
            Expr.binaryApp(ν(), e1, op, e2)
   )
}

const string_: Parser<ConstStr> =
   withAction(
      lexeme_(between(ch('"'), withJoin(repeat(stringCh)), ch('"'),)),
      lit => Expr.constStr(ν(), str(lit))
   )

// JSON grammar for numbers, https://tools.ietf.org/html/rfc7159.html#section-6.
// I'm assuming (but haven't checked that) DIGIT is defined as
const DIGIT: Parser<string> = range("0", "9")
// decimal-point = %x2E       ; .
const decimal_point = ch(".")
// digit1-9 = %x31-39         ; 1-9
const digit1to9: Parser<string> = range("1", "9")
// e = %x65 / %x45            ; e E
const e: Parser<string> = choice([ch("e"), ch("E")])
// minus = %x2D               ; -
const minus: Parser<string> = ch("-")
// plus = %x2B                ; +
const plus: Parser<string> = ch("+")
// zero = %x30                ; 0
const zero: Parser<string> = ch("0")
// exp = e [ minus / plus ] 1*DIGIT
const exp: Parser<string> = withJoin(sequence([e, optional(choice([minus, plus]), () => ""), withJoin(repeat1(DIGIT))]))
// frac = decimal-point 1*DIGIT
const frac = withJoin(sequence([decimal_point, withJoin(repeat1(DIGIT))]))
// int = zero / ( digit1-9 *DIGIT )
const int: Parser<string> = choice([zero, withJoin(sequence([digit1to9, withJoin(repeat(DIGIT))]))])
// number = [ minus ] int [ frac ] [ exp ]
const numberʹ: Parser<string> = 
   withJoin(sequence([optional(minus, () => ""), int, optional(frac, () => ""), optional(exp, () => "")]))

const number_: Parser<ConstNum> =
   withAction(lexeme_(numberʹ), lit => Expr.constNum(ν(), num(new Number(lit).valueOf())))

const parenthExpr: Parser<Expr> =
   parenthesise(expr)

const recDef: Parser<RecDef> =
   withAction(
      seq(dropFirst(keyword(strings.fun), var_), matches),
      ([name, σ]: [Str, Trie<Expr>]) =>
         Expr.recDef(ν(), name, σ)
   )

const recDefs1 : Parser<List<RecDef>> =
   withAction(sepBy1(recDef, symbol(";")), List.fromArray)

const let_: Parser<Let2> =
   withAction(
      dropFirst(keyword(strings.let_), seq(dropSecond(var_, symbol(strings.equals)), expr)),
      ([x, e]: [Str, Expr]) => Expr.let2(ν(), x, e)
   )

const letrec_: Parser<LetRec2> =
   withAction(
      dropFirst(keyword(strings.letRec), recDefs1),
      (δ: List<RecDef>) => 
         Expr.letRec2(ν(), δ)
   )

export const defList: Parser<List<Def>> =
   withAction(sepBy1(choice<Def>([let_, letrec_]), symbol(";")), List.fromArray)

const defs1 : Parser<Defs> =
   withAction(
      seq(defList, dropFirst(keyword(strings.in_), expr)),
      ([defs, e]: [List<Def>, Expr]) => Expr.defs(ν(), defs, e)
   )

// Enforce consistency with constructor signatures.
const constr: Parser<Constr> =
   withAction(
      seq(ctr, optional(parenthesise(sepBy1(expr, symbol(","))), () => [])),
      ([c, e̅]: [Str, Expr[]]) => {
         const n: number = arity(c)
         assert(n <= e̅.length,`Too few arguments to constructor ${c.val}.`)
         assert(n >= e̅.length, `Too many arguments to constructor ${c.val}.`)
         return Expr.constr(ν(), c, List.fromArray(e̅))
      }
   )

const listRestOpt: Parser<Expr> = 
   optional(dropFirst(seq(symbol(","), symbol("...")), expr), () => Expr.constr(ν(), str(Nil.name), nil()))

const listʹ: Parser<Constr> =
   optional(
      withAction(
         seq(sepBy1(expr, symbol(",")), listRestOpt),
         ([e̅, e]): Expr.Constr => {
            return [...e̅, e].reverse().reduce((e̅ʹ, eʹ) => {
               return Expr.constr(ν(), str(Cons.name), List.fromArray([eʹ, e̅ʹ]))
            }) as Expr.Constr
         }
      ),
      () => Expr.constr(ν(), str(Nil.name), nil())
   )

const list: Parser<Constr> =
   between(symbol(strings.bracketL), listʹ, symbol(strings.bracketR))

const pair: Parser<Constr> =
   withAction(
      parenthesise(seq(dropSecond(expr, symbol(",")), expr)),
      ([fst, snd]: [Expr, Expr]) =>
         Expr.constr(ν(), str(Pair.name), List.fromArray([fst, snd]))
   )

function args_pattern<K extends Kont<K>> (n: number, p: Parser<K>): Parser<Args<K>> {
   if (n === 0) {
      return withAction(p, Args.end)
   } else {
      let pʹ = args_pattern(n - 1, p)
      if (n > 1) {
         pʹ = dropFirst(symbol(","), pʹ)
      }
      return withAction(pattern(pʹ), Args.next)
   }
}

// Continuation-passing style means "parenthesise" idiom doesn't work here.
function constr_pattern<K extends Kont<K>> (p: Parser<K>): Parser<Trie.Constr<K>> {
   return withAction(
      seqDep(
         ctr, 
         (c: Str): Parser<Args<K>> => {
            const n: number = arity(c)
            if (n === 0) {
               return withAction(p, Args.end)
            } else {
               return dropFirst(symbol(strings.parenL), args_pattern(n, dropFirst(symbol(strings.parenR), p)))
            }
         }
      ),
      ([c, Π]: [Str, Args<K>]): Trie.Constr<K> =>
         Trie.constr(singleton(c, Π))
   )
}

// This was very hard to figure out; the types aren't helping as much as they should.
function listRest_pattern <K extends Kont<K>> (p: Parser<Args.End<K>>): Parser<Trie<Args.End<K>>> {
   return (state: ParseState) => 
      choice([
         dropFirst(symbol(","), dropFirst(symbol("..."), pattern(p))),
         dropFirst(symbol(","), list1_pattern(p)),
         withAction(p, (κ: Args.End<K>) => Trie.constr(singleton(str("Nil"), Args.end(κ))))
      ])(state)
}

function list1_pattern<K extends Kont<K>> (p: Parser<K>): Parser<Trie.Constr<K>> {
   return withAction(
      pattern(withAction(listRest_pattern(withAction(p, Args.end)), Args.next)),
      (σ: Trie<Args.Next<K>>) => Trie.constr(singleton(str("Cons"), Args.next(σ))) 
   )
}

function list_patternʹ<K extends Kont<K>> (p: Parser<K>): Parser<Trie.Constr<K>> {
   return choice([
      list1_pattern(p),
      withAction(p, (κ: K) => Trie.constr(singleton(str("Nil"), Args.end(κ))))
   ])
}

function list_pattern<K extends Kont<K>> (p: Parser<K>): Parser<Trie.Constr<K>> {
   return dropFirst(symbol(strings.bracketL), list_patternʹ(dropFirst(symbol(strings.bracketR), p)))
}

function pair_pattern<K extends Kont<K>> (p: Parser<K>): Parser<Trie.Constr<K>> {
   return withAction(
      dropFirst(symbol(strings.parenL), args_pattern(2, dropFirst(symbol(strings.parenR), p))),
      (Π: Args<K>): Trie.Constr<K> => Trie.constr(singleton(str("Pair"), Π))
   )
}

function variable_pattern<K extends Kont<K>> (p: Parser<K>): Parser<Trie.Var<K>> {
   return withAction(
      seq(var_, p), 
      ([x, κ]: [Str, K]): Trie.Var<K> => Trie.var_(x, κ)
   )
}

function pattern<K extends Kont<K>> (p: Parser<K>): Parser<Trie<K>> {
   return (state: ParseState) => 
      choice<Trie<K>>([variable_pattern(p), list_pattern(p), pair_pattern(p), constr_pattern(p)])(state)
}

const match: Parser<Trie<Expr>> =
   choice<Trie<Expr>>([
      pattern(dropFirst(symbol(strings.arrow), expr)),
      pattern(withAction(matches, (σ: Trie<Expr>): Expr => Expr.fun(ν(), σ)))
   ])

// Assume at least one match clause.
function matches (state: ParseState): ParseResult<Trie<Expr>> | null {
   return withAction(
      choice<Trie<Expr>[]>([
         withAction(match, m => [m]),
         between(symbol("{"), sepBy1(match, symbol(";")), symbol("}"))
      ]),
      (σ̅: Trie<Expr>[]) => {
         let σ: Trie<Expr> = σ̅[0]
         for (let i = 1; i < σ̅.length; ++i) {
            σ = Trie.Trie.join(σ, σ̅[i])
         } 
         return σ
      }
   )(state)
}

const matchAs: Parser<MatchAs> =
   withAction(
      seq(
         dropFirst(keyword(strings.match), expr),
         dropFirst(keyword(strings.as), matches)
      ),
      ([e, σ]: [Expr, Trie<Expr>]) => Expr.matchAs(ν(), e, σ)
   )

const fun: Parser<Fun> =
   withAction(
      dropFirst(keyword(strings.fun), matches),
      (σ: Trie<Expr>): Fun => Expr.fun(ν(), σ)
   )

// Any expression other than an operator tree or application chain.
const simpleExpr: Parser<Expr> =
   choice<Expr>([
      variable, string_, number_, parenthExpr, pair, defs1, list, constr, matchAs, fun
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
