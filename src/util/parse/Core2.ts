import { Ord, eq } from "../Ord"
import { __nonNull } from "../Core"
import { Value, _, make } from "../../Value2" // TODO: fix upwards dependency

export interface SyntaxNode {
}

// The parser builds a list of these. Currently interned, but will probably need to become versioned.
export abstract class Lexeme implements Value, SyntaxNode, Ord<Lexeme> {
   abstract str: string

   eq (l: Lexeme): boolean {
      return eq(this, l)
   }

   leq (l: Lexeme): boolean {
      return this.str <= l.str
   }

   toString (): string {
      return this.str
   }
}

export type SyntaxNodeOpt = SyntaxNode | null

export interface ParseResult<T extends SyntaxNodeOpt> {
   state: ParseState
   matched: string
   ast: T
}

export interface Parser<T extends SyntaxNodeOpt> {
   (state: ParseState): ParseResult<T> | null
}

export class Pos {
   line: number
   ch: number // call this 'ch' to implicitly implement CodeMirror.Position

   constructor( line: number, ch: number) {
      this.line = line
      this.ch = ch
   }

   toString (): string {
      return "(" + this.line + ", " + this.ch + ")"
   }

   equals (that: Pos): boolean {
      return this.line == that.line && this.ch === that.ch
   }
}

export type Span = [Pos, Pos]
export type Buffer = [Lexeme, Span][] // intuitively speaking

export function equals([from1, to1]: Span, [from2, to2]: Span): boolean {
   return from1.equals(from2) && to1.equals(to2)
}

export class ParseState {
   input: string
   pos: Pos
   index: number
   length: number
   buffer: Buffer // sequence of tokens recognised so far

   constructor (input: string, pos: Pos, index: number, buffer: Buffer) {
      this.input = input
      this.pos = pos
      this.index = index
      this.length = input.length - this.index
      this.buffer = buffer
   }

   from (index: number, nextLine: boolean = false): ParseState {
      const pos: Pos = nextLine
         ? new Pos(this.pos.line + 1, 0)
         : new Pos(this.pos.line, this.pos.ch + index)
      const r = new ParseState(this.input, pos, this.index + index, this.buffer)
      r.length = this.length - index
      return r
   }

   // TODO: assert that lexemes have contiguous spans, i.e. represent a linear consumption of the input.
   append ([l, [from, to]]: [Lexeme, Span]): ParseState {
      return new ParseState(this.input, this.pos, this.index, this.buffer.concat([[l, [from, to]]]))
   }

   // My first n characters.
   first (n: number): string {
      return this.input.substring(this.index, this.index + n)
   }

   at (index: number): string {
      return this.input.charAt(this.index + index)
   }
}

export function regExp (r: RegExp): Parser<string> {
   function regExp_ (state: ParseState): ParseResult<string> | null {
      const ms: string[] | null = state.first(state.length).match(r)
      if (ms !== null) {
         const s: string = ms[0]
         return { state: state.from(s.length, s === '\n'), matched: s, ast: s }
      } else {
         return null
      }
   }
   return regExp_
}

export class Whitespace extends Lexeme {
   str: string = _

   static make (str: string): Whitespace {
      return make(Whitespace, { str })
   }
}

export class SingleLineComment extends Lexeme {
   str: string = _

   static make (str: string): SingleLineComment {
      return make(SingleLineComment, { str })
   }
}

// Does this serve any purpose?
export class Operator extends Lexeme {
   str: string = _

   static make (str: string): Operator {
      return make(Operator, { str })
   }
}

export type LexemeClass<L extends Lexeme> = new () => L

function token<L extends Lexeme> (p: Parser<string>, C: LexemeClass<L>): Parser<L> {
   function token_ (state: ParseState): ParseResult<L> | null {
      const r: ParseResult<string> | null = p(state)
      if (r !== null) {
         const l: L = make(C, { str: r.ast } )
         return {
            state: r.state.append([l, [state.pos, r.state.pos]]),
            matched: r.matched,
            ast: l
         }
      }
      return null
   }
   return token_
}

// TODO: parameterise over this definition.
const horizWhitespace: Parser<string> = regExp(/^[ \f\t\r]+/)
const newLine: Parser<string> = regExp(/^\n/)
const newLines: Parser<string> = withAction(repeat1(newLine), strs => strs.join())
const whitespace: Parser<Whitespace> = token(choice([horizWhitespace, newLines]), Whitespace)
const singleLineComment: Parser<SingleLineComment> = token(regExp(/^\/\/.*/), SingleLineComment)
const ignore: Parser<Lexeme[]> = repeat(choice<Lexeme>([whitespace, singleLineComment]))

export function successfulParse<T extends SyntaxNode> (p: Parser<T>, str: string): T {
   return __nonNull(parse(p, str)).ast
}

// Match the supplied string with leading whitespace/comments, p, and then eof.
function parse<T extends SyntaxNode> (p: Parser<T>, str: string): ParseResult<T> | null {
   const p_: Parser<T> = withAction(
      seq(seq(ignore, p), eof),
      ([[_, t], eof]: [[Lexeme[], T], null]) => t
   )
   return p_(new ParseState(str, new Pos(0, 0), 0, []))
}

// As p, but transform its result by f.
export function withAction<T, U> (p: Parser<T>, f: (t: T) => U): Parser<U> {
   function withAction_(state: ParseState): ParseResult<U> | null {
      const r: ParseResult<T> | null = p(state)
      if (r !== null) {
         return { state: r.state, matched: r.matched, ast: f(r.ast) }
      }
      return null
   }
   return withAction_
}

// Combine a parser with an action which applies array "join" with an empty separator.
export function withJoin<T> (p: Parser<T[]>): Parser<string> {
   return withAction(p, (ast: T[]) => ast.join(''))
}

export function lexeme<L extends Lexeme> (p: Parser<string>, C: LexemeClass<L>): Parser<L> {
   return dropSecond(token(p, C), ignore)
}

// Parse a particular symbol.
export function symbol (s: string): Parser<Operator> {
   function symbol_ (state: ParseState): ParseResult<string> | null {
      if (state.length >= s.length && state.first(s.length) == s) {
         return { state: state.from(s.length), matched: s, ast: s }
      }
      return null
   }
   return lexeme(symbol_, Operator)
}

// Match a single specific character.
export function ch(c: string): Parser<string> {
   return range(c, c)
}

// Match a single character in an inclusive range ("a" to "z" for example).
// The AST node is the single-character string that was parsed.
export function range (lower: string, upper: string): Parser<string> {
   function range_ (state: ParseState): ParseResult<string> | null {
      if (state.length < 1)
         return null
      const ch: string = state.at(0)
      if (ch >= lower && ch <= upper)
         return { state: state.from(1), matched: ch, ast: ch }
      return null
   }
   return range_
}

// Negate a single-character parser. So negate(range('a', 'z')) will
// match anything except the characters in the range.
export function negate (p: Parser<string>): Parser<string> {
   function negate_ (state: ParseState): ParseResult<string> | null {
      if (state.length >= 1) {
         const r = p(state)
         if (r === null)
            return { state: state.from(1), matched: state.at(0), ast: state.at(0) }
         return null
      }
      return null
   }
   return negate_
}

// TODO: replace by existing foldl in BaseTypes.
export function foldLeft<T, U> (f: (x: T, y: U) => T, acc: T, xs: U[]): T {
   for (let i: number = 0; i < xs.length; ++i)
      acc = f(acc, xs[i])
   return acc
}

// Matches empty string only if there is no more input.
export function eof (state: ParseState): ParseResult<null> | null {
   if (state.length == 0)
      return { state: state, matched: '', ast: null }
   else
      return null
}

// Always succeeds, consuming no input and returning the supplied AST.
export function constant<T> (t: T): Parser<T> {
   function constant_(state: ParseState): ParseResult<T> {
      return { state: state, matched: '', ast: t }
   }
   return constant_
}

export const skip: Parser<null> = constant(null)

// Strict (non-lazy) sequential composition.
export function seq<T, U> (p1: Parser<T>, p2: Parser<U>): Parser<[T, U]> {
   return lazySeqDep(p1, (t: T) => p2)
}

export function seqDep<T, U> (p1: Parser<T>, p2: (t: T) => Parser<U>): Parser<[T, U]> {
   return lazySeqDep(p1, p2)
}

// Lazy sequential composition, for heterogeneously typed sequences. Matches both or neither,
// only instantiating the second parser if needed. Allow dependency of p2 on result of p1.
export function lazySeqDep<T, U> (p1: Parser<T>, p2: (t: T) => Parser<U>): Parser<[T, U]> {
   function lazySeq_(state: ParseState): ParseResult<[T, U]> | null {
      const result1: ParseResult<T> | null = p1(state)
      if (result1 === null) {
         return null
      } else {
         const result2: ParseResult<U> | null = __nonNull(p2(result1.ast))(result1.state)
         if (result2 === null) {
            return null
         } else {
            return { 
               state: result2.state, 
               matched: result1.matched + result2.matched, 
               ast: [result1.ast, result2.ast] 
            }
         }
      }
   }
   return lazySeq_
}

export function dropSecond<T, U> (p1: Parser<T>, p2: Parser<U>): Parser<T> {
   return withAction<[T, U], T>(seq(p1, p2), p => p[0])
}

export function dropFirst<T, U> (p1: Parser<T>, p2: Parser<U>): Parser<U> {
   return withAction<[T, U], U>(seq(p1, p2), p => p[1])
}

// Succeed only if all the parsers in the supplied sequence succeed. Return an array of
// their ASTs.
export function sequence<T> (ps: Parser<T>[]): Parser<T[]> {
   return sequence_<T>(ps, 0)
}

// Auxiliary function for sequence<T>, which additionally takes an index into the array.
function sequence_<T> (ps: Parser<T>[], i: number): Parser<T[]> {
   if (i === ps.length) {
      return constant<T[]>([])
   }
   return withAction<[T, T[]], T[]>(
      seq(ps[i], sequence_(ps, i + 1)),
      p => [p[0]].concat(p[1])
   )
}

// Tries each of the given parsers in order, succeeding at the first successful parse,
// failing otherwise.
export function choice<T> (ps: Parser<T>[]): Parser<T> {
   function choice_(state: ParseState): ParseResult<T> | null {
      for (let i: number = 0; i < ps.length; ++i) {
         const result: ParseResult<T> | null = ps[i](state)
         if (result)
            return result
      }
      return null
   }
   return choice_
}

// Succeeds if p1 matches and p2 does not, or p1 matches and the matched text is
// longer than p2's. Useful for things like: butnot(identifier, reservedWord).
export function butnot<T, U> (p1: Parser<T>, p2: Parser<U>): Parser<T> {
   function butnot_(state: ParseState): ParseResult<T> | null {
      const br: ParseResult<U> | null = p2(state)
      if (br === null) {
         return p1(state)
      } else {
         const ar: ParseResult<T> | null = p1(state)
         if (ar !== null && ar.matched.length > br.matched.length)
            return ar
         else
            return null
      }
   }
   return butnot_
}

// Zero or more matches of another parser, returning their results as an array.
export function repeat<T> (p: Parser<T>): Parser<T[]> {
   return optional(repeat1(p), () => [])
}

// One or more matches of p.
export function repeat1<T> (p: Parser<T>): Parser<T[]> {
   return withAction(
      lazySeqDep(p, () => repeat(p)),
      ([t, ts]: [T, T[]]) => [t].concat(ts)
   )
}

// Zero or one match of p, returning the AST supplied by t if there is no match. The default value
// is supplied by a function as it may be need to be created by a side-effecting function.
export function optional<T> (p: Parser<T>, t: () => T): Parser<T> {
   function optional_(state: ParseState): ParseResult<T> | null {
      return p(state) || constant<T>(t())(state)
   }
   return optional_
}

export function satisfying<T> (p: Parser<T>, pred: (ast: T) => boolean): Parser<T> {
   function satisfying_(state: ParseState): ParseResult<T> | null {
      const r: ParseResult<T> | null = p(state)
      if (r !== null && pred(r.ast))
         return r
      return null
   }
   return satisfying_
}

export function between <T1, T, T2>(p1: Parser<T1>, p: Parser<T>, p2: Parser<T2>): Parser<T> {
   return dropFirst(p1, dropSecond(p, p2))
}

// Discards the ASTs associated with the separators.
export function sepBy1<T, U>( p: Parser<T>, sep: Parser<U>): Parser<T[]> {
   return withAction(
      seq(p, repeat(dropFirst(sep, p))),
      ([t, ts]: [T, T[]]): T[] => [t].concat(ts)
   )
}

// One or more occurrences of p, separated by applications of an operator. Returns the value
// obtained by left-associative application of successive functions returned by appOp to
// successive values returned by p.
export function chainl1<T> (p: Parser<T>, appOp: Parser<(x: T, y: T) => T>): Parser<T> {
   return withAction(
      seq(p, repeat(seq(appOp, p))),
      ([f, t]: [T, [(x: T, y: T) => T, T][]]) =>
         foldLeft(
            (v: T, [f, t]: [(x: T, y: T) => T, T]) => f(v, t),
            f,
            t
         )
   )
}
