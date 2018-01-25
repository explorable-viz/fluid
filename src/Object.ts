// This mustn't be a module, otherwise it doesn't extend the global Object type. A better
// way to do this might be to avoid extending the built-in Object.

// Strings ok for small, flat keys; for large/nested keys we'll need trees.
type Addr = string

interface Object {
   __history: this[]
   __version (): void
   __addr?: Addr
}
