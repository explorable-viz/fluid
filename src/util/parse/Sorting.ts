import { Comparator } from "../Ord"
import { Lexeme, Buffer } from "./Core"

export function occursBefore(buffer: Buffer): Comparator<Lexeme> {
   return (l1: Lexeme, l2: Lexeme): number => {
      const line1: number = lineNumber(buffer, l1)
      const line2: number = lineNumber(buffer, l2)
      if (line1 === line2) {
         return 0
      } else
      if (line1 > line2) {
         return 1
      } else {
         return -1
      }
   }
}

function lineNumber(buffer: Buffer, l: Lexeme): number {
   for (const [l_, [from, _]] of buffer) {
      if (l === l_) {
         return from.line
      }
   }

   // primitives (e.g. Math) have lexemes that are not in the source
   return 0
}
