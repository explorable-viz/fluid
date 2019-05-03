import { Value } from "./ExplVal2"

export type Env = { 
   [x: string]: Value 
}

export function concat (ρ1: Env, ρ2: Env): Env {
   throw new Error
}

export function empty (): Env {
   throw new Error
}

export function singleton (x: string, v: Value): Env {
   throw new Error
}
