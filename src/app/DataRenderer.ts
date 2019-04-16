import { __nonNull, absurd, as } from "../util/Core"
import { PersistentObject } from "../util/Persistent"
import { Cons, List, Nil, Pair } from "../BaseTypes"
import { AnnNumber, AnnString } from "../Graphics"

export type Data = List<Pair<AnnNumber | AnnString, PersistentObject>> // approximate recursive type

abstract class Token {
   abstract text: string
   abstract fillStyle: string
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
}

class Line {
   tokens: [number, Token][]

   constructor () {
      this.tokens = []
   }
}

class Presentation {
   ctx: CanvasRenderingContext2D
   indentx: number
   lines: Line[]

   constructor (ctx: CanvasRenderingContext2D) {
      this.ctx = ctx
      this.indentx = 0
      this.lines = []
   }

   newLine (indentx: number): void {
      this.lines.push(new Line)
      this.indentx = indentx
   }

   push (token: Token): void {
      this.indentx += this.ctx.measureText(token.text).width
      this.lines[this.lines.length - 1].tokens.push([this.indentx, token])
   }

   draw (lineHeight: number): void {
      this.lines.forEach((line: Line, n: number): void => {
         line.tokens.forEach(([x, token]) => {
            this.ctx.fillStyle = token.fillStyle
            this.ctx.fillText(token.text, x, (n + 1) * lineHeight)
         })
      })
   }
}

export class DataRenderer {
   ctx: CanvasRenderingContext2D
   lineHeight: number
   lines: number

   constructor (canvas: HTMLCanvasElement) {
      this.ctx = __nonNull(canvas.getContext("2d"))
      this.ctx.font = "10pt Arial"
      this.ctx.textAlign = "left"
      this.ctx.textBaseline = "middle"
      // No easy way to access text height, but this will do for now.
      // https://stackoverflow.com/questions/1134586
      this.lineHeight = this.ctx.measureText("M").width
      canvas.addEventListener("mousemove", (e: MouseEvent): void => {
           e.clientX
           e.clientY
      })
   }

   render (data: Data): void {
      this.lines = 0
      const pres: Presentation = new Presentation(this.ctx)
      this.renderData(0, data, pres)
//      this.ctx.clearRect(0, 0, this.ctx.canvas.width, this.ctx.canvas.height)
      pres.draw(this.lineHeight)
   }

   renderData (indentx: number, data: Data, pres: Presentation): void {
      if (Cons.is(data)) {
         ++this.lines
         pres.newLine(indentx)
         const { fst: key, snd: val }: Pair<AnnNumber | AnnString, PersistentObject> = as(data.head, Pair)
         let keyStr: string
         this.ctx.fillStyle = key.α ? "black" : "red"
         if (key instanceof AnnNumber) {
            keyStr = key.n.toString()
            pres.push(new AnnNumberToken(key))
         } else
         if (key instanceof AnnString) {
            keyStr = key.str
            pres.push(new AnnStringToken(key))
         } else {
            return absurd()
         }
         keyStr += ": "
         pres.push(new StringToken(": "))
         this.ctx.fillText(keyStr, indentx, this.lines * this.lineHeight)
         const newIndentx = indentx + this.ctx.measureText(keyStr).width
         let valStr: string
         if (val instanceof List) {
            this.renderData(newIndentx, val as Data, pres)
         } else 
         if (val instanceof AnnNumber || val instanceof AnnString) {
            this.ctx.fillStyle = val.α ? "black" : "red"
            if (val instanceof AnnNumber) {
               valStr = val.n.toString()
            } else {
               valStr = val.str
            }
            this.ctx.fillText(valStr, newIndentx, this.lines * this.lineHeight)
         } else {
            return absurd()
         }
         this.renderData(indentx, data.tail, pres)
      } else
      if (Nil.is(data)) {
         return
      } else {
         return absurd()
      }
   }
}

