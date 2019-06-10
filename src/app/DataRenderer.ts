import { __nonNull, absurd, error } from "../util/Core"
import { Cons, Nil, Pair } from "../BaseTypes"
import { DataValue } from "../DataValue"
import { Direction } from "../Eval"
import { Expr } from "../Expr"
import { _, make } from "../Value"
import { asVersioned } from "../Versioned"
import { Slicer } from "./GraphicsRenderer"

abstract class Token extends DataValue<"Token"> {
   abstract text: string
   abstract fillStyles: [string, string]
}

abstract class AnnotatedToken extends Token {
   abstract clearAnnotation (): void
   abstract setAnnotation (): void
}

class NumToken extends AnnotatedToken {
   n: Expr.ConstNum = _

   get text (): string {
      return this.n.val.toString()
   }

   get fillStyles (): [string, string] {
      return __nonNull(this.n.__α) ? ["black", "blue"] : ["blue", "black"]
   }

   clearAnnotation (): void {
      this.n.__α = false
   }

   setAnnotation (): void {
      this.n.__α = true
   }
}

function numToken (n: Expr.ConstNum): NumToken {
   return make(NumToken, n)
}

class StrToken extends AnnotatedToken {
   str: Expr.ConstStr = _

   get text (): string {
      return this.str.val.val
   }

   get fillStyles (): [string, string] {
      return __nonNull(this.str.__α) ? ["black", "blue"] : ["blue", "black"]
   }

   clearAnnotation (): void {
      this.str.__α = false
   }

   setAnnotation (): void {
      this.str.__α = true
   }
}

function strToken (str: Expr.ConstStr): StrToken {
   return make(StrToken, str)
}

class StringToken extends Token {
   str: string = _

   get text (): string {
      return this.str
   }

   get fillStyles (): [string, string] {
      return ["black", "black"]
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
   slicer: Slicer

   constructor (ctx: CanvasRenderingContext2D, lineHeight: number, slicer: Slicer) {
      this.ctx = ctx
      this.lineHeight = lineHeight
      this.indentx = this.width = 0
      this.lines = []
      this.lastMouseToken = null
      this.slicer = slicer
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
            this.ctx.fillStyle = this.slicer.direction === Direction.Fwd ? token.fillStyles[0] : token.fillStyles[1]
            this.ctx.fillText(token.text, x, (n + 1) * this.lineHeight)
         })
      })
   }

   get height (): number {
      return this.lines.length * this.lineHeight
   }

   // Maintain invariant that the /only/ token annotated with false is the one with mouse focus.
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

   constructor (ctx: CanvasRenderingContext2D, data: Expr, slicer: Slicer) {
      // for some reason setting font doesn't change font size but only affects spacing :-/
      ctx.textAlign = "left"
      // No easy way to access text height, but this will do for now.
      // https://stackoverflow.com/questions/1134586
      this.view = new DataView(ctx, ctx.measureText("M").width * 1.4, slicer)
      this.renderData(0, data)
   }

   // Data must have recursive format:
   // Row = Pair<Num | Str, Data>
   // Data = List<Row> 
   renderData (indentx: number, data: Expr): void {
      if (data instanceof Expr.Constr && data.ctr.val === Cons.name) {
         this.view.newLine(indentx)
         const row: Expr = data.args.toArray()[0] // head
         if (row instanceof Expr.Constr && row.ctr.val === Pair.name) {
            const [key, val]: Expr[] = row.args.toArray()
            if (key instanceof Expr.ConstNum) {
               this.view.push(numToken(key))
            } else
            if (key instanceof Expr.ConstStr) {
               this.view.push(strToken(key))
            } else {
               error("Data format error: expected Num or Str expression.")
            }
            this.view.push(stringToken(": "))
            if (val instanceof Expr.Constr && (data.ctr.val === Cons.name || data.ctr.val === Nil.name)) {
               this.renderData(this.view.indentx, val)
            } else 
            if (val instanceof Expr.ConstNum) {
               this.view.push(numToken(asVersioned(val)))
            } else
            if (val instanceof Expr.ConstStr) {
               this.view.push(strToken(asVersioned(val)))
            } else {
               error("Data format error: expected List, Num or Str expression.")
            }
            this.renderData(indentx, data.args.toArray()[1]) // tail
         } else {
            error("Data format error: expected Pair expression.")
         }
      } else
      if (data instanceof Expr.Constr && data.ctr.val === Nil.name) {
         return
      } else {
         absurd()
      }
   }
}
