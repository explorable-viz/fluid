import { Annotated } from "../util/Annotated"
import { Class, __check, __nonNull, absurd, as, assert, className, classOf } from "../util/Core"
import { Persistent, PersistentObject, make } from "../util/Persistent"
import { asVersioned, at, fieldVals } from "../util/Versioned"
import { Cons, Pair, List, Nil } from "../BaseTypes"
import { arity } from "../DataType"
import { ExplVal, Value } from "../ExplVal"
import { AnnNumber, AnnString, Graphic, PathStroke, Point, RectFill, Scale, Transform, Translate, Transpose } from "../Graphics"

// intermediate value required to stop TS getting confused:
const classFor_: Class<PersistentObject>[] =
   [Cons,
    Graphic,
    Nil,
    PathStroke,
    Pair,
    Point,
    RectFill,
    Transform,
    Scale,
    Translate,
    Transpose],
   classFor: Map<string, Class<PersistentObject>> = new Map(
      classFor_.map((cls): [string, Class<PersistentObject>] => [__nonNull(cls).name, cls])
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
   const k: Reflect = Reflect.make(v)
   if (v instanceof Value.ConstNum) {
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
   } else 
   if (v instanceof Value.Closure) {
      return absurd("Unexpected closure; too few arguments to function?")
   } else {
      return absurd()
   }
}

export function reify (v: Persistent): Value {
   const vʹ: Value = as(asVersioned(v).__id, Reflect).v
   if (v instanceof AnnNumber) {
      vʹ.setα(v.α)
   } else
   if (v instanceof AnnString) {
      vʹ.setα(v.α)
   } else {
      if (v instanceof Annotated) {
         assert(classFor.get(className(v)) === classOf(v))
         fieldVals(v).slice(1).forEach(reify) // slice to skip initial annotations field
         vʹ.setα(v.α)
      } else {
         return absurd()
      }
   }
   return vʹ
}
