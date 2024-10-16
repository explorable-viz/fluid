import { Pair, Tree } from "./BaseTypes";
import { Persistent, Str } from "./Value";
export declare type FiniteMap<V> = Tree<Pair<Str, V>>;
export declare function get<V extends Persistent>(m: FiniteMap<V>, k: Str): V | undefined;
export declare function insert<V extends Persistent>(m: FiniteMap<V>, k: Str, v: V): FiniteMap<V>;
export declare function singleton<V extends Persistent>(k: Str, v: V): FiniteMap<V>;
export declare function unionWith<V extends Persistent, T extends FiniteMap<V>>(m1: T, m2: T, f: (v1: V, v2: V) => V): T;
