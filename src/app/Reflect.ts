import { Annotation, Annotated } from "../util/Annotated"
import { Class, __check, absurd, as, assert, funName } from "../util/Core"
import { Persistent, PersistentObject, make } from "../util/Persistent"
import { at } from "../util/Versioned"
import { Cons, List, Nil } from "../BaseTypes"
import { arity } from "../DataType"
import { ExplVal, Value } from "../ExplVal"
import { PathStroke, Point, RectFill, Translate } from "../Graphics"

// Reflected versions of primitive constants; should be able to switch to a compiler and use these directly.
// Can't extend built-in classes because they require initialisation at construction-time.

export class AnnNumber extends Annotated implements PersistentObject {
   n: number

   constructor_ (α: Annotation, n: number) {
      this.α = α
      this.n = n
   }
}

export class AnnString extends Annotated implements PersistentObject {
   str: string

   constructor_ (α: Annotation, str: string) {
      this.α = α
      this.str = str
   }
}

// intermediate value required to stop TS getting confused:
const classFor_: Class<PersistentObject>[] =
   [Cons,
    Nil,
    PathStroke,
    Point,
    RectFill,
    Translate],
   classFor: Map<string, Class<PersistentObject>> = new Map(
      classFor_.map((cls): [string, Class<PersistentObject>] => [funName(cls), cls])
   )

// TODO: use function objects themselves to partition memo keys, as per lambdacalc-old?
class Reflect implements PersistentObject {
   v: Value

   constructor_ (v: Value) {
      this.v = v
   }

   static make (v: Value): Reflect {
      return make(Reflect, v)
   }
}

export function reflect (v: Value): Persistent { 
// if (!v.α) {
//    console.log(v)
// }
   const k: Reflect = Reflect.make(v)
   if (v instanceof Value.ConstInt) {
      return at(k, AnnNumber, v.α, v.val)
   } else
   if (v instanceof Value.ConstStr) {
      return at(k, AnnString, v.α, v.val)
   } else
   if (v instanceof Value.Constr) {
      const ctr: string = __check(v.ctr.str, it => classFor.has(it)),
            args: Persistent[] = []
      for (let tvs: List<ExplVal> = v.args; Cons.is(tvs);) {
         args.push(reflect(tvs.head.v))
         tvs = tvs.tail
      }
      assert(args.length === arity(ctr))
      // α doesn't appear as argument of user-level data types; sanity-check that reflective counterpart expects it
      return as(at(k, classFor.get(ctr)!, v.α, ...args), Annotated)
   } else {
      return absurd()
   }
}
    