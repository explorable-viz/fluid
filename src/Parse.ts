import { 
   Parser, ParseResult, ParseState, between, butnot, ch, chainl1, choice, constant, dropFirst,
   dropSecond, lexeme, negate, optional, range, repeat, repeat1, satisfying, sepBy1, seq, sequence, 
   symbol, withAction, withJoin
} from "./util/parse/Core"
import { Traced, ν } from "./Runtime"
import { Lex, Trace, join, str } from "./Syntax"
import { 
   App, ConstInt, ConstStr, Constr, ConstrTrie, EmptyBody, EmptyTrace, Fun, Let, LetRec, MatchAs,
   OpName, RecBinding, RecDefinition, Trie, Value, Var, VarTrie
} from "./Syntax"

// General convention: define parsers 'pointfully' (as functions), rather than as combinator expressions,
// whenever the recursive nature of the grammar causes a problem with variable initialisation.
export module Parse {

function newExpr (t: Trace): Traced {
   return Traced.at(ν(), t, null)
}

function __val <V extends Value> (v: V): Traced<V> {
   return Traced.at(ν(), EmptyTrace.at(ν()), v)
}

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
   return (state: ParseState): ParseResult<string> => {
      const r: ParseResult<string> = identCandidate(state)
      if (r !== null && r.ast === str)
         return r
      return null
   }
}

const ctr: Parser<Lex.Ctr> = 
   lexeme(satisfying(identCandidate, isCtr), Lex.Ctr)

// Note that primitive operations that have names (e.g. reflect, intToString) are /exactly/ like regular
// identifiers. They can be shadowed, for example.
const var_: Parser<Lex.Var> =
   lexeme(
      butnot(satisfying(identCandidate, str => !isCtr(str)), reservedWord),
      Lex.Var
   )

const variable: Parser<Traced> =
   withAction(var_, (x: Lex.Var) => newExpr(Var.at(ν(), x)))

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

const productOp: Parser<Traced> =
   withAction(
      satisfying(opCandidate, isProductOp),
      opName => newExpr(OpName.at(ν(), opName))
   )

const sumOp: Parser<Traced> =
   withAction(
      satisfying(opCandidate, isSumOp),
      opName => newExpr(OpName.at(ν(), opName))
   )

const compareOp: Parser<Traced> =
   withAction(
      satisfying(opCandidate, opName => !isProductOp(opName) && !isSumOp(opName)),
      opName => newExpr(OpName.at(ν(), opName))
   )

const symbolOp: Parser<Traced> = 
   choice<Traced>([productOp, sumOp, compareOp])

function parenthesise<T> (p: Parser<T>): Parser<T> {
   return between(symbol(str.parenL), p, symbol(str.parenR))
}

// We permit Haskell-style "sections" in the surface syntax for convenience, but internally there is
// no distinction between sections and infix.
const sectionOp: Parser<Traced> = parenthesise(symbolOp)

// Consume no input, because application is represented simply by adjacency.
const app_: Parser<(e1: Traced, e2: Traced) => Traced> =
   withAction(
      constant(null),
      (_: Traced) =>
         (e1: Traced, e2: Traced): Traced =>
            newExpr(App.at(ν(), e1, e2, EmptyBody.at(ν())))
   )

function appOp (
   opP: Parser<Traced>
): Parser<(e1: Traced, e2: Traced) => Traced> {
   return withAction(
      opP,
      op =>
         (e1: Traced, e2: Traced): Traced =>
            newExpr(
               App.at(ν(),
                  newExpr(App.at(ν(), op, e1, EmptyBody.at(ν()))),
                  e2,
                  EmptyBody.at(ν())
               )
            )
   )
}

const string_: Parser<Traced<ConstStr>> =
   withAction(
      lexeme(between(ch('"'), withJoin(repeat(stringCh)), ch('"'),), Lex.StringLiteral),
      lit => __val(ConstStr.at(ν(), lit.str))
   )

const integer: Parser<Traced<ConstInt>> =
   withAction(
      lexeme(
         choice<string>([
            decimalDigits,
            withJoin(sequence([ch("+"), decimalDigits])),
            withJoin(sequence([ch("-"), decimalDigits]))
         ]),
         Lex.IntLiteral
      ),
      lit => __val(ConstInt.at(ν(), parseInt(lit.str)))
   )

const parenthExpr: Parser<Traced> = 
   parenthesise(expr)

const let_: Parser<Traced> =
   withAction(
      seq(
         dropFirst(keyword(str.let_), seq(dropSecond(var_, symbol(str.equals)), expr)),
         dropFirst(keyword(str.in_), expr)
      ),
      ([[x, e], eʹ]: [[Lex.Var, Traced], Traced]) =>
         Traced.at(ν(), Let.at(ν(), e, VarTrie.at(ν(), x, eʹ)), eʹ.val)
   )

const recDefinition: Parser<RecBinding> =
   withAction(
      seq(dropFirst(keyword(str.fun), var_), matches),
      ([name, σ]: [Lex.Var, Trie<Traced>]) =>
         RecBinding.at(ν(), RecDefinition.at(ν(), name, Fun.at(ν(), σ)), null)
   )

const letrec: Parser<Traced> =
   withAction(
      seq(
         dropFirst(keyword(str.letRec), repeat1(recDefinition)),
         dropFirst(keyword(str.in_), expr)
      ),
      ([defs, body]: [RecBinding[], Traced]) =>
         Traced.at(ν(), LetRec.at(ν(), defs, body.trace), body.val)
   )

const constr: Parser<Traced> =
   withAction(
      seq(ctr, optional(parenthesise(sepBy1(expr, symbol(","))), [])),
      ([ctr, args]: [Lex.Ctr, Traced[]]) =>
         __val(Constr.at(ν(), ctr, args))
   )

const pair: Parser<Traced<Constr>> =
   withAction(
      parenthesise(seq(dropSecond(expr, symbol(",")), expr)),
      ([fst, snd]: [Traced, Traced]) =>
         __val(Constr.at(ν(), new Lex.Ctr("Pair"), [fst, snd]))
   )

function args_pattern (p: Parser<Object>): Parser<Trie<Object>> {
   return (state: ParseState) => 
      pattern(choice([dropFirst(symbol(","), args_pattern(p)), p]))(state)
}

// Continuation-passing style means 'parenthesise' idiom doesn't work here.
function constr_pattern (p: Parser<Object>): Parser<ConstrTrie<Object>> {
   return withAction(
      seq(
         ctr, 
         choice([dropFirst(symbol(str.parenL), args_pattern(dropFirst(symbol(str.parenR), p))), p])
      ),
      ([ctr, z]: [Lex.Ctr, Traced]) => ConstrTrie.at(ν(), new Map([[ctr.str, z]])) 
   )
}

function pair_pattern (p: Parser<Object>): Parser<ConstrTrie<Object>> {
   return withAction(
      dropFirst(
         symbol(str.parenL), 
         pattern(dropFirst(symbol(","), pattern(dropFirst(symbol(str.parenR), p))))
      ),
      (σ: Trie<Traced>) => ConstrTrie.at(ν(), new Map([["Pair", σ]]))
   )
}

function variable_pattern (p: Parser<Object>): Parser<VarTrie<Object>> {
   return withAction(
      seq(var_, p), ([x, z]: [Lex.Var, Traced]) => 
         VarTrie.at(ν(), x, z)
      )
}

// Wasn't able to figure out the trie type parameters. Using Object allows us not to care.
function pattern (p: Parser<Object>): Parser<Trie<Object>> {
   return (state: ParseState) => 
      choice<Trie<Traced>>([variable_pattern(p), pair_pattern(p), constr_pattern(p)])(state)
}

// Chain of singleton tries, terminating in an expression.
const match: Parser<Trie<Traced>> = 
   pattern(dropFirst(symbol(str.arrow), expr))

// Assume at least one match clause.
function matches (state: ParseState): ParseResult<Trie<Traced>> {
   return withAction(
      choice<Trie<Traced>[]>([
         withAction(match, m => [m]),
         between(symbol("{"), sepBy1(match, symbol(";")), symbol("}"))
      ]),
      (σs: Trie<Traced>[]) => {
         let σ: Trie<Traced> = σs[0]
         for (let i = 1; i < σs.length; ++i) {
            σ = join(σ, σs[i])
         } 
         return σ
      }
   )(state)
}

const matchAs: Parser<Traced> =
   withAction(
      seq(
         dropFirst(keyword(str.match), expr),
         dropFirst(keyword(str.as), matches)
      ),
      ([e, σ]: [Traced, Trie<Traced>]) =>
         newExpr(MatchAs.at(ν(), e, σ))
   )

const fun: Parser<Traced> =
   withAction(
      dropFirst(keyword(str.fun), matches),
      (σ: Trie<Traced>) => newExpr(Fun.at(ν(), σ))
   )

// Any expression other than an operator tree or application chain.
const simpleExpr: Parser<Traced> =
   choice<Traced>([
      variable, string_, integer, sectionOp, parenthExpr, pair, let_, letrec, constr, matchAs, fun
   ])

// A left-associative tree, with applications at the branches, and simple terms at the leaves.
const appChain: Parser<Traced> = chainl1(simpleExpr, app_)

// An expression is an operator tree. An operator tree is a tree whose branches are infix
// binary primitives and whose leaves are application chains.
const productExpr: Parser<Traced> = chainl1(appChain, appOp(productOp))
const sumExpr: Parser<Traced> = chainl1(productExpr, appOp(sumOp))
const compareExpr: Parser<Traced> = chainl1(sumExpr, appOp(compareOp))

export function expr (state: ParseState): ParseResult<Traced> {
   return compareExpr(state)
}

}
