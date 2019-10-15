import { Class, __nonNull, assert } from "./util/Core"
import { Delta, Change, __deltas } from "./Delta"
import { Id, Memoisable, MemoTable, Persistent, Num, Str, Value, _, construct, fields, make, memoCall } from "./Value"

// Versioned objects are persistent objects that have state that varies across worlds. Interface because the 
// same datatype can be interned in some contexts and versioned in others.
export type Versioned<T> = Versioned_ & T

// Why do versioned objects need to store their id?
export interface Versioned_ {
   __id: Id
   __ẟ: Delta
}

export function versioned<T extends Value> (v: T): v is Versioned<T> {
   return (v as Versioned<T>).__id !== undefined
}

export function asVersioned<T extends Value> (v: T): Versioned<T> {
   if (versioned(v)) {
      return v
   } else {
      return assert(false, `${v} is not versioned.`)
   }
}

// For versioned objects the map is not curried but takes an (interned) composite key. This stores only "derived"
// (internal) versioned nodes, not external.
type VersionedValues = Map<Id, Versioned<Value>>
const __versioned: VersionedValues = new Map

// The (possibly already extant) versioned object uniquely identified by a memo-key. As an idempotent side-effect,
// record how the object differs from its previous version.
export function at<T extends Value> (C: Class<T>, ...v̅: Persistent[]): (k: Id) => Versioned<T> {
   return (k: Id) => {
      let v: Versioned<Value> | undefined = __versioned.get(k)
      if (v === undefined) {
         const v: Versioned<T> = create(C, ...v̅)(k)
         if (!(k instanceof Extern)) {
            __versioned.set(k, v)
         }
         return v
      } else {
         assert(!(k instanceof Extern)) // "external" nodes are always created fresh
         reset(v, C, ...v̅)
         return v as Versioned<T>
      }
   }
}

export function create<T extends Value> (C: Class<T>, ...v̅: Persistent[]): (k: Id) => Versioned<T> {
   return (k: Id) => {
      const v: Versioned<T> = new C as Versioned<T>
      Object.defineProperty(v, "__id", {
         value: k,
         enumerable: false
      })
      Object.defineProperty(v, "__ẟ", {
         // The delta map is partial; the absence of an entry is equivalent to an empty delta. This allows
         // deltas to be cleared simply by removing all entries from the map.
         get: function (): Delta {
            let ẟ: Delta | undefined = __deltas.ẟ̅.get(this)
            if (ẟ === undefined) {
               ẟ = new Change({})
               __deltas.ẟ̅.set(this, ẟ)
               return ẟ
            } else {
               return ẟ
            }
         },
         enumerable: false
      })
      __deltas.created(v, construct(true, v, v̅)!)
      return v
   }
}

export function reset<T extends Value> (v: Value, C: Class<T>, ...v̅: Persistent[]): void {
   if (v instanceof C) {
      __deltas.changed(v, construct(true, v, v̅)!)
   } else {
      reclassify(v, C)
      __deltas.reclassified(v, construct(true, v, v̅)!)
   }
}

// Should emulate the post-state of "new C". Probably need to worry about how this works with inherited properties.
function reclassify<T extends Value> (v: Value, ctr: Class<T>): void {
   const proto: Object = Object.getPrototypeOf(new ctr)
   assert (Object.getPrototypeOf(v) !== proto)
   for (const k of fields(v)) {
      assert(delete v[k as keyof Object])
   }
   Object.setPrototypeOf(v, proto)
}

// A memo key which is sourced externally to the system. (The name "External" is already taken.)
export class Extern extends Id {
   id: number = _
}

function extern (id: number): Extern {
   return make(Extern, id)
}

// Fresh keys represent inputs to the system, e.g. addresses of syntax nodes provided by an external structure editor.
export const ν: () => Extern =
   (() => {
      let count: number = 0
      return () => {
         return extern(count++)
      }
   })()

export function num (val: number): (k: Id) => Versioned<Num> {
   return at(Num, val)
}

export function str (val: string): (k: Id) => Versioned<Str> {
   return at(Str, val)
}

// Memo-entries for functions are not invariant across revisions, so clear memo table at each revision.
const __funMemo: MemoTable = new Map

export function newRevision (): void {
   __funMemo.clear()
   __deltas.clear()
}

export type MemoFunType<T extends Persistent> = (...v̅: Persistent[]) => T

class MemoFun<T extends Persistent> implements Memoisable<T> {
   f: MemoFunType<T>

   constructor (f: MemoFunType<T>) {
      this.f = f
   }

   get key (): Persistent {
      return this.f
   }

   call (v̅: Persistent[]): T {
      return this.f.apply(null, v̅)
      // for an "instance" version where v̅[0] is "this" use:
      // return this.f.apply(v̅[0], v̅.slice(1))
   }
}

// Memoisation.
export function memo<T extends Persistent> (f: MemoFunType<T>, ...v̅: Persistent[]): T {
   return memoCall(__funMemo, new MemoFun(f), v̅)
}
