import { __nonNull, as } from "../util/Core"
import { PersistentObject } from "../util/Persistent"
import { Cons, List, Nil, Pair } from "../BaseTypes"
import { AnnNumber, AnnString } from "../Graphics"

export type Data = List<Pair<AnnNumber | AnnString, PersistentObject>> // approximate recursive type

abstract class Token {
   abstract text: string
   abstract fillStyle: string

   abstract onMouseEnter (): void
   abstract onMouseExit (): void
}

class AnnNumberToken extends Token {
   n: AnnNumber

   constructor (n: AnnNumber) {
      super()
      this.n = n
   }

   get text (): string {
      return this.n.n.toString()
   }

   get fillStyle (): string {
      return this.n.α ? "black" : "red"
   }

   onMouseEnter (): void {
      this.n.setα(false)
   }

   onMouseExit (): void {
      this.n.setα(true)
   }
}

class AnnStringToken extends Token {
   str: AnnString

   constructor (str: AnnString) {
      super()
      this.str = str
   }

   get text (): string {
      return this.str.str
   }

   get fillStyle (): string {
      return this.str.α ? "black" : "red"
   }

   onMouseEnter (): void {
      this.str.setα(false)
   }

   onMouseExit (): void {
      this.str.setα(true)
   }
}

class StringToken extends Token {
   str: string

   constructor (str: string) {
      super()
      this.str = str
   }

   get text (): string {
      return this.str
   }

   get fillStyle (): string {
      return "black"
   }

   onMouseEnter (): void {
   }

   onMouseExit (): void {
   }
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
   lastMouseToken: Token | null 

   constructor (ctx: CanvasRenderingContext2D, lineHeight: number) {
      this.ctx = ctx
      this.lineHeight = lineHeight
      this.indentx = this.width = 0
      this.lines = []
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

   onMouseMove (x: number, y: number): void {
      const line: Line = this.lines[Math.floor(y / this.lineHeight)]
      let token: Token | null = null
      for (let [xʹ, tokenʹ] of line.tokens) {
         if (xʹ > x) {
            break
         }
         token = tokenʹ
      }
      if (token !== this.lastMouseToken) {
         if (token !== null) {
            token.onMouseEnter()
         }
         if (this.lastMouseToken !== null) {
            this.lastMouseToken.onMouseExit()
         }
         this.lastMouseToken = token
         console.log(token !== null ? token.text : "(no token)")
      }
   }
}

export class DataRenderer {
   view: DataView

   constructor (canvas: HTMLCanvasElement, data: Data) {
      const ctx: CanvasRenderingContext2D = __nonNull(canvas.getContext("2d"))
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
         const { fst: key, snd: val }: Pair<AnnNumber | AnnString, PersistentObject> = as(data.head, Pair)
         if (key instanceof AnnNumber) {
            this.view.push(new AnnNumberToken(key))
         } else {
            this.view.push(new AnnStringToken(key))
         }
         this.view.push(new StringToken(": "))
         if (val instanceof List) {
            this.renderData(this.view.indentx, val as Data)
         } else 
         if (val instanceof AnnNumber || val instanceof AnnString) {
            if (val instanceof AnnNumber) {
               this.view.push(new AnnNumberToken(val))
            } else {
               this.view.push(new AnnStringToken(val))
            }
         }
         this.renderData(indentx, data.tail)
      } else
      if (Nil.is(data)) {
         return
      }
   }
}
