import { __nonNull } from "./Util"

export interface ParseResult<T> {
   remaining: ParseState;
   matched: string;
   ast: T;
}

export interface Parser<T> {
   (state: ParseState): ParseResult<T> | null;
}

export class ParseState {
   input: string;
   index: number;
   length: number;

   constructor (input: string, index: number) {
      this.input = input;
      this.index = index;
      this.length = input.length - this.index;
   }

   from (index: number): ParseState {
      const r = new ParseState(this.input, this.index + index);
      r.length = this.length - index;
      return r
   }

   // My first n characters.
   first (n: number): string {
      return this.input.substring(this.index, this.index + n)
   }

   at (index: number): string {
      return this.input.charAt(this.index + index)
   }
}

// Parse a token which matches a regex. It's a bit weird to do this, seeing as we have a
// library for parsing and lexical analysis. But treating comments properly requires a
// little bit of effort (see the 'whitespace' function in GenTokenParser in Haskell's Parsec
// library for an example). For now this will do.
function regExp (r: RegExp): Parser<string> {
   return (state: ParseState): ParseResult<string> => {
      const ms: string[] | null = state.first(state.length).match(r);
      if (ms !== null) {
         const s: string = ms[0];
         return { remaining: state.from(s.length), matched: s, ast: s }
      } else {
         return null
      }
   }
}

// TODO: make the parsing infrastructure parametric in these definitions.
const multiLineCommentDelimiter = { start: '\{-', end: '-\}' }

const multiLineComment: Parser<string> = regExp(new RegExp(
   '^' + multiLineCommentDelimiter.start + '(.|[\r\n])*?' + multiLineCommentDelimiter.end
))

const whitespace: Parser<string> = regExp(/^\s+/)

// Match the supplied string with p, and then eof.
export function parse<T> (p: Parser<T>, str: string): ParseResult<T> {
   const p_: Parser<T> = withAction(
      seq(seq(lexeme(constant(null)), p), eof),
      (result: [[string, T], void]): T => result[0][1]
   )
   return p_(new ParseState(str, 0))
}

// Match the lexical parser p and any trailing whitespace/comments, returning the result of p.
export function lexeme (p: Parser<string>): Parser<string> {
   return withAction(
      seq(p, repeat(choice([whitespace, multiLineComment]))),
      pair => pair[0]
   )
}

// Parse a particular symbol. The AST contains the symbol that was parsed, as a string.
function symbol (s: string): Parser<string> {
   return (state: ParseState): ParseResult<string> => {
      if (state.length >= s.length && state.first(s.length) == s) {
         return { remaining: state.from(s.length), matched: s, ast: s }
      } else {
         return null
      }
   }
}

export function token (s: string): Parser<string> {
   return lexeme(symbol(s))
}

// Match a single specific character.
export function ch (c: string): Parser<string>	{
   return range(c, c)
}

// Match a single character in an inclusive range ("a" to "z" for example).
// The AST contains the single-character string that was parsed.
export function range (lower: string, upper: string): Parser<string> {
   return (state: ParseState): ParseResult<string> => {
      if (state.length < 1)
         return null
      const ch: string = state.at(0);
      if(ch >= lower && ch <= upper)
         return { remaining: state.from(1), matched: ch, ast: ch }
      return null
   }
}

// Negate a single-character parser. So negate(range('a', 'z')) will
// match anything except the characters in the range.
export function negate (p: Parser<string>): Parser<string> {
   return (state: ParseState): ParseResult<string> => {
      if (state.length >= 1) {
         const r = p(state);
         if (r === null)
            return { remaining: state.from(1), matched: state.at(0), ast: state.at(0) }
         return null
      }
      return null
   }
}

// As p, but transform its result by f.
export function withAction<T, U> (p: Parser<T>, f: (t: T) => U): Parser<U> {
   return (state: ParseState): ParseResult<U> => {
      const r: ParseResult<T> = p(state);
      if (r !== null) {
         return { remaining: r.remaining, matched: r.matched, ast: f(r.ast) }
      }
      return null
   }
}

// Combine a parser with an action which applies array "join" with an empty separator.
export function withJoin<T> (p: Parser<T[]>): Parser<string> {
   return withAction(p, (ast: T[]) => ast.join(''))
}

// TODO: replace by existing foldl in BaseTypes.
function foldLeft<T, U> (f: (x: T, y: U) => T, acc: T, xs: U[]): T {
   for (let i: number = 0; i < xs.length; ++i)
      acc = f(acc, xs[i])
   return acc
}

// Matches empty string only if there is no more input.
function eof (state: ParseState): ParseResult<void> {
   if (state.length == 0)
      return { remaining: state, matched: '', ast: null }
   else
      return null
}

// Always succeeds, consuming no input and returning the supplied AST.
export function constant <T> (t: T): Parser<T> {
   return (state: ParseState): ParseResult<T> => {
      return { remaining: state, matched: '', ast: t }
   }
}

// Strict (non-lazy) sequential composition.
export function seq <T, U> (p1: Parser<T>, p2: Parser<U>): Parser<[T, U]> {
   return lazySeq(p1, () => p2)
}

// Lazy sequential composition, for heterogeneously typed sequences. Matches both or neither,
// only instantiating the second parser if needed.
export function lazySeq <T, U> (p1: Parser<T>, p2: () => Parser<U>): Parser<[T, U]> {
   return (state: ParseState): ParseResult<[T, U]> => {
      let t: T = null,
          u: U = null
      const result: ParseResult<[T, U]> = { remaining: null, matched: '', ast: null },
            result1: ParseResult<T> = p1(state)
      if (result1 === null)
         return null
      else {
         result.remaining = result1.remaining
         t = result1.ast
         result.matched += result1.matched
      }
      const result2: ParseResult<U> = __nonNull(p2())(result.remaining)
      if (result2 === null)
         return null
      else {
         result.remaining = result2.remaining
         u = result2.ast
         result.matched += result2.matched
      }
      result.ast = [t, u]
      return result
   }
}

export function dropSecond <T, U> (p1: Parser<T>, p2: Parser<U>): Parser<T> {
   return withAction<[T, U], T>(seq(p1, p2), p => p[0])
}

export function dropFirst <T, U> (p1: Parser<T>, p2: Parser<U>): Parser<U> {
   return withAction<[T, U], U>(seq(p1,p2), p => p[1])
}

// Succeed iff the supplied parsers succeed in sequence. Return an array of their ASTs.
export function sequence<T> (ps: Parser<T>[]): Parser<T[]> {
   return sequence_<T>(ps, 0)
}

// Auxiliary function for sequence<T>, which additionally takes an index into the array.
function sequence_<T> (ps: Parser<T>[], i: number): Parser<T[]> {
   if (i === ps.length) {
      return constant<T[]>([])
   }
   return withAction<[T, T[]],T[]>(
      seq(ps[i], sequence_(ps, i + 1)),
      p => [p[0]].concat(p[1])
   )
}

// Tries each of the given parsers in order. The first one that succeeds
// results in a successful parse. Fails only if all parsers fail.
export function choice<T> (ps: Parser<T>[]): Parser<T> {
   return (state: ParseState): ParseResult<T> => {
      for (let i: number = 0; i < ps.length; ++i) {
         const result: ParseResult<T> = ps[i](state)
         if (result)
            return result
      }
      return null
   }
}

// Succeeds if p1 matches and p2 does not, or p1 matches and the matched text is
// longer than p2's. Useful for things like: butnot(identifier, reservedWord).
export function butnot<T, U> (p1: Parser<T>, p2: Parser<U>): Parser<T> {
   return (state: ParseState): ParseResult<T> => {
      const br: ParseResult<U> = p2(state);
      if (br === null) {
         return p1(state)
      } else {
         const ar: ParseResult<T> = p1(state);
         if (ar && ar.matched.length > br.matched.length)
            return ar
         else
            return null
      }
   }
}

// Zero or more matches of another parser, returning their results as an array.
export function repeat<T> (p: Parser<T>): Parser<T[]> {
   return optional(repeat1(p), [])
}

// One or more matches of p.
export function repeat1<T> (p: Parser<T>): Parser<T[]> {
   return withAction(
      lazySeq(p, () => repeat(p)),
      (args: [T, T[]]): T[] => [args[0]].concat(args[1])
   )
}

// Zero or one match of p, returning the supplied AST if there is no match.
export function optional<T> (p: Parser<T>, t: T): Parser<T> {
   return (state: ParseState): ParseResult<T> => {
      return p(state) || constant<T>(t)(state)
   }
}

export function satisfying<T> (p: Parser<T>, pred: (ast: T) => boolean): Parser<T> {
   return (state: ParseState): ParseResult<T> => {
      const r: ParseResult<T> = p(state);
      if (r !== null && pred(r.ast))
         return r
      return null
   }
}

export function between<T1, T, T2> (p1: Parser<T1>, p: Parser<T>, p2: Parser<T2>): Parser<T> {
   return dropFirst(p1, dropSecond(p, p2))
}

// Discards the ASTs associated with the separators.
export function sepBy1<T, U> (p: Parser<T>, sep: Parser<U>): Parser<T[]> {
   return withAction(
      seq(p, repeat(dropFirst(sep, p))),
      (args: [T, T[]]): T[] => [args[0]].concat(args[1])
   )
}

// One or more occurrences of p, separated by applications of an operator. Returns the value
// obtained by left-associative application of successive functions returned by appOp to
// successive values returned by p.
export function chainl1<T> (p: Parser<T>, appOp: Parser<(x: T, y: T) => T>): Parser<T> {
   return withAction(
      seq(p, repeat(seq(appOp, p))),
      (args: [T, [(x: T, y: T) => T, T][]]) =>
         foldLeft(
            (v: T, args: [(x: T, y: T) => T, T]): T => args[0](v, args[1]),
            args[0],
            args[1]
         )
   )
}
