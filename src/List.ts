import { make } from "./util/Core"
import { PersistentObject } from "./Runtime"

// Interned lists of persistent objects.
export abstract class List<T extends PersistentObject> extends PersistentObject {
   __List (): void {
      // discriminator
   }

   abstract length: number
}

export class Nil<T extends PersistentObject> extends List<T> { 
   static is<T extends PersistentObject> (xs: List<T>): xs is Nil<T> {
      return xs instanceof Nil
   }

   static make<T extends PersistentObject> (): Nil<T> {
      return make(Nil)
   }

   get length (): number {
      return 0
   }
}

export class Cons<T extends PersistentObject> extends List<T> {
   head: T
   tail: List<T>

   static is<T extends PersistentObject> (xs: List<T>): xs is Cons<T> {
      return xs instanceof Cons
   }

   static make<T extends PersistentObject> (head: T, tail: List<T>): Cons<T> {
      const this_: Cons<T> = make<Cons<T>>(Cons, head, tail)
      this_.head = head
      this_.tail = tail
      return this_
   }

   get length (): number {
      return 1 + this.tail.length
   }
}
