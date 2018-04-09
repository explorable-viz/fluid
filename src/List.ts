import { make } from "./util/Core"
import { PersistentObject } from "./Runtime"

// Interned lists of persistent objects.
export class List<T extends PersistentObject> extends PersistentObject {
   __List (): void {
      // discriminator
   }
}

export class Nil<T extends PersistentObject> extends List<T> { 
   static make<T extends PersistentObject> (): Nil<T> {
      return make(Nil)
   }
}

export class Cons<T extends PersistentObject> extends List<T> {
   head: T
   tail: List<T>

   static make<T extends PersistentObject> (head: T, tail: List<T>): Cons<T> {
      const this_: Cons<T> = make<Cons<T>>(Cons, head, tail)
      this_.head = head
      this_.tail = tail
      return this_
   }
}
