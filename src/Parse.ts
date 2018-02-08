import { /*Int, None, Pair, __toList*/ } from "./BaseTypes"
// import { singleton } from "./FiniteMap"
// import { keyP } from "./Memo"
import { 
   Parser, ParseResult, ParseState, /*between, */butnot, ch, chainl1, choice, /*constant, dropFirst, */
   /* dropSecond,*/ lexeme, /*negate, optional,*/ range, repeat, repeat1, satisfying,/* sepBy1, seq,*/ sequence, 
   symbol, withAction, withJoin
} from "./util/parse/Core"
import { Traced /*, __tracedK, create*/, ν } from "./Runtime"
import { Lex, Trace/*, join*/, str } from "./Syntax"
import * as AST from "./Syntax"
// import { className, make } from "./util/Core"

// General convention: define parsers 'pointfully' (as functions), rather than as combinator expressions,
// whenever the recursive nature of the grammar causes a problem with variable initialisation.
export module Parse {

function newExpr <T extends Trace> (t: T): Traced<T> {
   return Traced.at(ν(), t, null)
}
/*
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
*/
function keyword(str: string): Parser<Lex.Keyword> {
   return lexeme(reserved(str), Lex.Keyword)
}

// No Unicode support for identifiers yet.
const identStart: Parser<string> =
   choice<string>([range('a', 'z'), range('A', 'Z'), ch('$'), ch('_')])

const identPart: Parser<string> =
   choice<string>([identStart, range('0', '9')])

const identEnd: Parser<string> =
   ch('\'')

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
/*
const ctr: Parser<Str> = 
   satisfying(identCandidate, isCtr)

// Note that primitive operations that have names (e.g. reflect, intToString) are /exactly/ like regular
// identifiers. They can be shadowed, for example.
const ident: Parser<Str> =
   withAction(
      butnot(satisfying(identCandidate, str => !isCtr(str)), reservedWord),
      (x: string) => Str.at(ν(), x)
   )

const variable: Parser<ITraced<AST.Var>> =
   withAction(ident, (x: Str) => newExpr(AST.Var.at(ν(), __val(ν(), x))))

// Only allow Unicode escape sequences (i.e. no hex or octal escapes, nor "character" escapes such as \r).
const hexDigit: Parser<string> = 
   choice<string>([range('0', '9'), range('a', 'f'), range('A', 'F')])

const unicodeEscape: Parser<string> =
   withAction(
      sequence([ch('x'), hexDigit, hexDigit, hexDigit, hexDigit]),
      chars => String.fromCharCode(parseInt('0x' + chars.join('').substring(1)))
   )

// Standard Java/C escape sequences. They happen to be a subset of JavaScript's escape sequences, so
// this just defines the embedding.
const singleCharEscape: Parser<string> = choice<string>([
   withAction(ch('\''), _ => '\''),
   withAction(ch('"'), _ => '\"'),
   withAction(ch('\\'), _ => '\\'),
   withAction(ch('b'), _ => '\b'),
   withAction(ch('f'), _ => '\f'),
   withAction(ch('n'), _ => '\t'),
   withAction(ch('r'), _ => '\r'),
   withAction(ch('t'), _ => '\t')
])

const escapeSeq: Parser<string> =
   dropFirst(ch('\\'), choice<string>([unicodeEscape, singleCharEscape]))

const stringCh: Parser<string> =
   choice<string>([negate(choice<string>([ch('\"'), ch('\\'), ch('\r'), ch('\n')])), escapeSeq])

const decimalDigits: Parser<string> = 
   withJoin(repeat1(range('0', '9')))
*/
// To avoid having to deal with arbitrary operator precedence, we classify all operators as one of three
// kinds, depending on the initial character. See 0.5.1 release notes.
const opCandidate: Parser<Lex.OpName> =
   lexeme(
      butnot(
         withJoin(repeat1(choice([ch('+'), ch('*'), ch('/'), ch('-'), ch('='), ch('<'), ch('>')]))),
         symbol(str.equals)
      ),
      Lex.OpName
   )

function isProductOp (opName: Lex.OpName): boolean {
   return opName.str.charAt(0) === '*' || opName.str.charAt(0) === '/'
}

function isSumOp (opName: Lex.OpName): boolean {
   return opName.str.charAt(0) === '+' || opName.str.charAt(0) === '-'
}

const productOp: Parser<Traced> =
   withAction(
      satisfying(opCandidate, isProductOp),
      opName => newExpr(AST.OpName.at(ν(), opName))
   )

const sumOp: Parser<Traced> =
   withAction(
      satisfying(opCandidate, isSumOp),
      opName => newExpr(AST.OpName.at(ν(), opName))
   )

const compareOp: Parser<Traced> =
   withAction(
      satisfying(opCandidate, opName => !isProductOp(opName) && !isSumOp(opName)),
      opName => newExpr(AST.OpName.at(ν(), opName))
   )
/*
const symbolOp: Parser<ITraced> = 
   choice<ITraced>([productOp, sumOp, compareOp])

function parenthesise<T> (p: Parser<T>): Parser<T> {
   return between(token(str.parenL), p, token(str.parenR))
}

// We permit Haskell-style "sections" in the surface syntax for convenience, but internally there is
// no distinction between sections and infix.
const sectionOp: Parser<ITraced> = parenthesise(symbolOp)

// Consume no input, because application is represented simply by adjacency.
const app_: Parser<(e1: ITraced, e2: ITraced) => ITraced> =
   withAction(
      constant(null),
      (_: ITraced) =>
         (e1: ITraced, e2: ITraced): ITraced =>
            newExpr(AST.App.at(ν(), __val(ν(), e1), __val(ν(), e2), __val(ν(), AST.EmptyBody.at(ν()))))
   )
*/
function appOp (
   opP: Parser<Traced>
): Parser<(e1: Traced, e2: Traced) => Traced> {
   return withAction(
      opP,
      op =>
         (e1: Traced, e2: Traced): Traced =>
            newExpr(
               AST.App.at(ν(),
                  newExpr(AST.App.at(ν(), op, e1, AST.EmptyBody.at(ν()))),
                  e2,
                  AST.EmptyBody.at(ν())
               )
            )
   )
}
/*
const string_: Parser<ITraced<Str>> =
   withAction(
      lexeme(between(ch('\"'), withJoin(repeat(stringCh)), ch('\"'))),
      str => __val(ν(), Str.at(ν(), str))
   )

const integer: Parser<ITraced<Int>> =
   withAction(
      lexeme(
         choice<string>([
            decimalDigits,
            withJoin(sequence([ch('+'), decimalDigits])),
            withJoin(sequence([ch('-'), decimalDigits]))
         ]),
         Lex.Int
      ),
      str => __val(ν(), Int.at(ν(), parseInt(str)))
   )

const parenthExpr: Parser<ITraced> = 
   parenthesise(expr)

const let_: Parser<ITraced> =
   withAction(
      seq(
         dropFirst(keyword(str.let_), seq(dropSecond(ident, token(str.letInitSep)), expr)),
         dropFirst(keyword(str.in_), expr)
      ),
      ([[x, e], eʹ]: [[Str, ITraced], ITraced]) =>
         __tracedK(ν(), AST.Let.at(ν(), __val(ν(), e), __val(ν(), AST.VarTrie.at(ν(), __val(ν(), x), eʹ))), eʹ.val)
   )

const recDefinition: Parser<AST.RecBinding> =
   withAction(
      seq(dropFirst(keyword(str.fun), ident), matches),
      ([name, σ]: [Str, AST.Trie<ITraced>]) =>
         AST.RecBinding.at(ν(),
            __val(ν(), AST.RecDefinition.at(ν(), __val(ν(), name), __val(ν(), AST.Fun.at(ν(), __val(ν(), σ))))),
            __val(ν(), None.at<AST.Closure>(ν()))
         )
   )

const letrec: Parser<ITraced> =
   withAction(
      seq(
         dropFirst(keyword(str.letRec), repeat1(recDefinition)),
         dropFirst(keyword(str.in_), expr)
      ),
      ([defs, body]: [AST.RecBinding[], ITraced]) =>
         __tracedK(ν(), AST.LetRec.at(ν(), __val(ν(), __toList(defs)), __val(ν(), body.trace)), body.val)
   )

const constr: Parser<ITraced> =
   withAction(
      seq(ctr, optional(parenthesise(sepBy1(expr, token(','))), [])),
      ([ctr, args]: [string, ITraced[]]) =>
         __val(ν(), reflect(AST.Constr.at(ν(), __val(ν(), ctr), __val(ν(), __toList(args)))))
   )

// Redundantly use reflection here to force everything through the same infrastructure.
const pair: Parser<ITraced<Pair<ITraced, ITraced>>> =
   withAction(
      parenthesise(seq(dropSecond(expr, token(',')), expr)),
      ([fst, snd]: [ITraced, ITraced]) => {
         const p: AST.Constr = AST.Constr.at(
            ν(), 
            __val(ν(), Str.at(ν(), className(Pair))), 
            __val(ν(), __toList([fst, snd]))
         )
         return __val(ν(), <Pair<ITraced, ITraced>>reflect(p))
      })

function args_pattern (p: Parser<Object>): Parser<AST.Trie<Object>> {
   return (state: ParseState) => 
      pattern(choice([dropFirst(token(','), args_pattern(p)), p]))(state)
}

// Continuation-passing style means 'parenthesise' idiom doesn't work here.
function constr_pattern (p: Parser<Object>): Parser<AST.ConstrTrie<Object>> {
   return withAction(
      seq(
         ctr, 
         choice([dropFirst(token(str.parenL), args_pattern(dropFirst(token(str.parenR), p))), p])
      ),
      ([ctr, z]: [string, Object]) =>
         AST.ConstrTrie.at(ν(), __val(ν(), singleton(ctr, z))) 
   )
}

function pair_pattern (p: Parser<Object>): Parser<AST.ConstrTrie<Object>> {
   return withAction(
      dropFirst(
         token(str.parenL), 
         pattern(dropFirst(token(','), pattern(dropFirst(token(str.parenR), p))))
      ),
      (σ: AST.Trie<Object>) => AST.ConstrTrie.at(ν(), __val(ν(), singleton(Str.at(ν(), className(Pair)), σ)))
   )
}

function variable_pattern (p: Parser<Object>): Parser<AST.VarTrie<Object>> {
   return withAction(seq(ident, p), ([x, z]: [Str, Object]) => 
      AST.VarTrie.at(ν(), __val(ν(), x), __val(ν(), z)))
}

function pattern (p: Parser<Object>): Parser<AST.Trie<Object>> {
   return (state: ParseState) =>
      choice<AST.Trie<ITraced>>([variable_pattern(p), pair_pattern(p), constr_pattern(p)])(state)
}

// Chain of singleton tries, terminating in an expression.
const match: Parser<AST.Trie<Object>> = 
   pattern(dropFirst(token(str.matchBodySep), expr))

// Assume at least one match clause.
function matches (state: ParseState): ParseResult<AST.Trie<Object>> {
   return withAction(
      choice<AST.Trie<ITraced>[]>([
         withAction(match, m => [m]),
         between(token('{'), sepBy1(match, token(';')), token('}'))
      ]),
      (σs: AST.Trie<ITraced>[]) => {
         let σ: AST.Trie<ITraced> = σs[0]
         for (let i = 1; i < σs.length; ++i) {
            σ = join(σ, σs[i])
         } 
         return σ
      }
   )(state)
}

const matchAs: Parser<ITraced> =
   withAction(
      seq(
         dropFirst(keyword(str.match), expr),
         dropFirst(keyword(str.as), matches)
      ),
      ([e, σ]: [ITraced, AST.Trie<ITraced>]) =>
         newExpr(AST.MatchAs.at(ν(), __val(ν(), e), __val(ν(), σ)))
   )

const fun: Parser<ITraced> =
   withAction(
      dropFirst(keyword(str.fun), matches),
      (σ: AST.Trie<ITraced>) => newExpr(AST.Fun.at(ν(), __val(ν(), σ)))
   )

// Any expression other than an operator tree or application chain.
const simpleExpr: Parser<ITraced> =
   choice<ITraced>([
      variable, string_, integer, sectionOp, parenthExpr, pair, let_, letrec, constr, matchAs, fun
   ])
*/

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
