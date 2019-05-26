import { as } from "../util/Core"
import { Cons, List, Nil, Pair } from "../BaseTypes2"
import { DataValue } from "../DataValue2"
import { Num, Str, Value, make } from "../Value2"
import { Versioned, asVersioned } from "../Versioned2"

type Row = Pair<Num | Str, Value> // approximate recursive type
export type Data = List<Row> 

abstract class Token extends DataValue<"Token"> {
   abstract text: string
   abstract fillStyle: string
}

abstract class AnnotatedToken extends Token {
   abstract clearAnnotation (): void
   abstract setAnnotation (): void
}

class NumToken extends AnnotatedToken {
   n: Versioned<Num> = _

   get text (): string {
      return this.n.val.toString()
   }

   get fillStyle (): string {
      return this.n.__α ? "black" : "red"
   }

   clearAnnotation (): void {
      console.log(`Clearing annotation on ${this.text}`)
      this.n.__α = false
   }

   setAnnotation (): void {
      this.n.__α = true
   }
}

function numToken (n: Versioned<Num>): NumToken {
   return make(NumToken, n)
}

class StrToken extends AnnotatedToken {
   str: Versioned<Str> = _

   get text (): string {
      return this.str.val
   }

   get fillStyle (): string {
      return this.str.__α ? "black" : "red"
   }

   clearAnnotation (): void {
      console.log(`Clearing annotation on ${this.text}`)
      this.str.__α = false
   }

   setAnnotation (): void {
      this.str.__α = true
   }
}

function strToken (str: Versioned<Str>): StrToken {
   return make(StrToken, str)
}

class StringToken extends Token {
   str: string

   constructor_ (str: string) {
      this.str = str
   }

   get text (): string {
      return this.str
   }

   get fillStyle (): string {
      return "black"
   }
}

function stringToken (str: string): StringToken {
   return make(StringToken, str)
}

class Line {
   tokens: [number, Token][]

   constructor () {
      this.tokens = []
   }
}

export class DataView {
   ctx: CanvasRenderingContext2D
   lineHeight: number
   indentx: number
   lines: Line[]
   width: number
   lastMouseToken: AnnotatedToken | null 

   constructor (ctx: CanvasRenderingContext2D, lineHeight: number) {
      this.ctx = ctx
      this.lineHeight = lineHeight
      this.indentx = this.width = 0
      this.lines = []
      this.lastMouseToken = null
   }

   newLine (indentx: number): void {
      this.lines.push(new Line)
      this.indentx = indentx
   }

   push (token: Token): void {
      this.lines[this.lines.length - 1].tokens.push([this.indentx, token])
      this.indentx += this.ctx.measureText(token.text).width
      this.width = Math.max(this.width, this.indentx)
   }

   draw (): void {
      this.lines.forEach((line: Line, n: number): void => {
         line.tokens.forEach(([x, token]) => {
            this.ctx.fillStyle = token.fillStyle
            this.ctx.fillText(token.text, x, (n + 1) * this.lineHeight)
         })
      })
   }

   get height (): number {
      return this.lines.length * this.lineHeight
   }

   // Return whether any annotations changed.
   onMouseMove (x: number, y: number): boolean {
      const line: Line = this.lines[Math.floor(y / this.lineHeight)]
      let token: Token | null = null
      for (let [xʹ, tokenʹ] of line.tokens) {
         if (xʹ > x) {
            break
         }
         token = tokenʹ
      }
      if (token !== this.lastMouseToken && token instanceof AnnotatedToken) {
         token.clearAnnotation()
         if (this.lastMouseToken !== null) {
            this.lastMouseToken.setAnnotation()
         }
         this.lastMouseToken = token
         return true
      } else {
         return false
      }
   }
}

export class DataRenderer {
   view: DataView

   constructor (ctx: CanvasRenderingContext2D, data: Data) {
      // for some reason setting font doesn't change font size but only affects spacing :-/
      ctx.textAlign = "left"
      // No easy way to access text height, but this will do for now.
      // https://stackoverflow.com/questions/1134586
      this.view = new DataView(ctx, ctx.measureText("M").width * 1.4)
      this.renderData(0, data)
   }

   renderData (indentx: number, data: Data): void {
      if (Cons.is(data)) {
         this.view.newLine(indentx)
         const { fst: key, snd: val }: Row = as(data.head, Pair)
         if (key instanceof Num) {
            this.view.push(numToken(asVersioned(key)))
         } else {
            this.view.push(strToken(asVersioned(key)))
         }
         this.view.push(stringToken(": "))
         if (val instanceof List) {
            this.renderData(this.view.indentx, val as Data)
         } else 
         if (val instanceof Num || val instanceof Str) {
            if (val instanceof Num) {
               this.view.push(numToken(asVersioned(val)))
            } else {
               this.view.push(strToken(asVersioned(val)))
            }
         }
         this.renderData(indentx, data.tail)
      } else
      if (Nil.is(data)) {
         return
      }
   }
}
