export type RawId = number

export class Id {
   __Id() {
      // descriminator
   }
}

export class PersistentObject<T extends Id> extends Object {
   // Initialise these properties at object creation.
   __history: this[] = undefined as any
   __id: T = undefined as any
   __version: () => Object = undefined as any
}
