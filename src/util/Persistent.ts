// An object which can be used as a key in an ES6 map (i.e. one for which equality is ===). In particular
// interned objects are persistent objects. Interface so can be extended by VersionedObject, which it is
// convenient to have as an interface.
export interface PersistentObject {
   // ES6 only allows constructor calls via "new".
   constructor_ (...v̅: MemoArgs): void
}

// Functions are persistent to support primitives.
export type Persistent = PersistentObject | boolean | string | number | Function

// Curried map from constructors and arguments to cached values; curried because composite keys would 
// require either custom equality, which isn't possible with ES6 maps, or interning, which would essentially
// involve the same memoisation logic.
type MemoTable = Map<Persistent, Persistent | Map<Persistent, Object>> // approximate recursive type
const __memoTable: MemoTable = new Map

function lookupArg<T extends Persistent> (f: Memoisable<T>, m: MemoTable, v̅: MemoArgs, n: number): Persistent | Map<Persistent, Object> {
   // for memoisation purposes, treat f's key as argument -1
   const k: Persistent = n === -1 ? f.key : v̅[n]
   let v: Persistent | Map<Persistent, Object> | undefined = m.get(k)
   if (v === undefined) {
      if (n === v̅.length - 1) {
         v = f.call(v̅)
         v = v! // TS confused: think v can be undefined here
      } else {
         v = new Map
      }
      m.set(k, v)
   }
   return v
}

export type PersistentClass<T extends PersistentObject = PersistentObject> = new () => T

// Unify memo-functions and interned classes.
interface Memoisable<T extends Persistent> {
   key: Persistent
   call (args: MemoArgs): T
}

class MemoCtr<T extends PersistentObject> implements Memoisable<T> {
   ctr: PersistentClass<T>

   constructor (ctr: PersistentClass<T>) {
      this.ctr = ctr
   }

   get key (): Persistent {
      return this.ctr
   } 

   call (v̅: MemoArgs): T {
      const o: T = new this.ctr
      o.constructor_(...v̅)
      Object.freeze(o)
      return o
   }
}

export type MemoFunType<T extends Persistent> = (...v̅: MemoArgs) => T
export type MemoArgs = Persistent[]

class MemoFun<T extends Persistent> implements Memoisable<T> {
   f: MemoFunType<T>

   constructor (f: MemoFunType<T>) {
      this.f = f
   }

   get key (): Persistent {
      return this.f
   }

   call (v̅: MemoArgs): T {
      return this.f.apply(null, v̅)
      // for an "instance" version where v̅[0] is "this" use:
      // return this.f.apply(v̅[0], v̅.slice(1))
   }
}

export function memoCall<T extends Persistent> (f: Memoisable<T>, v̅: MemoArgs): T {
   let v: Persistent | Map<Persistent, Object> = lookupArg(f, __memoTable, v̅, -1)
   for (let n: number = 0; n < v̅.length; ++n) {
      // since there are more arguments, the last v was a (nested) map
      v = lookupArg(f, v as MemoTable, v̅, n)
   }
   return v as T
}

// Hash-consing (interning) object construction.
export function make<T extends PersistentObject> (ctr: PersistentClass<T>, ...v̅: MemoArgs): T {
   return memoCall(new MemoCtr(ctr), v̅)
}

// Memoisation.
export function memo<T extends Persistent> (f: MemoFunType<T>, ...v̅: MemoArgs): T {
   return memoCall(new MemoFun(f), v̅)
}
