import { __nonNull, absurd, as } from "../util/Core"
import { PersistentObject } from "../util/Persistent"
import { Cons, List, Nil, Pair } from "../BaseTypes"
import { AnnNumber, AnnString } from "../Graphics"

export type Data = List<Pair<AnnNumber | AnnString, PersistentObject>> // approximate recursive type

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
   }

   render (data: Data): void {
      this.lines = 0
      this.renderData(0, data)
   }

   renderData (indentx: number, data: Data): void {
      if (Cons.is(data)) {
         ++this.lines
         const { fst: key, snd: val }: Pair<AnnNumber | AnnString, PersistentObject> = as(data.head, Pair)
         let keyStr: string
         this.ctx.fillStyle = key.α ? "black" : "red"
         if (key instanceof AnnNumber) {
            keyStr = key.n.toString()
         } else
         if (key instanceof AnnString) {
            keyStr = key.str
         } else {
            return absurd()
         }
         keyStr += ": "
         this.ctx.fillText(keyStr, indentx, this.lines * this.lineHeight)
         const newIndentx = indentx + this.ctx.measureText(keyStr).width
         let valStr: string
         if (val instanceof List) {
            this.renderData(newIndentx, val as Data)
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
         this.renderData(indentx, data.tail)
      } else
      if (Nil.is(data)) {
         return
      } else {
         return absurd()
      }
   }
}

